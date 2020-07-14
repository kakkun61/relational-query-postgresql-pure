{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Relational.PostgreSQL.Pure.TH (
  makeRelationalRecord,
  makeRelationalRecord',

  defineTableDefault',
  defineTableDefault,

  defineTableFromDB',
  defineTableFromDB,

  inlineVerifiedQuery
  ) where

import           Control.Monad                          (void, when)
import           Data.Functor.ProductIsomorphic.TH      (reifyRecordType)
import qualified Data.Map                               as Map
import           Data.Maybe                             (fromMaybe, listToMaybe)
import           Data.String                            (fromString)

import           Database.HDBC                          (SqlValue)
import           Database.PostgreSQL.Pure               (ColumnInfo, Connection, Oid, disconnect, parse, sync)

import           Language.Haskell.TH                    (Dec, Name, Q, Type (AppT, ConT), TypeQ, runIO)
import           Language.Haskell.TH.Lib.Extra          (reportError, reportWarning)
import           Language.Haskell.TH.Name.CamelCase     (varCamelcaseName)

import           Database.Record.TH                     (defineSqlPersistableInstances, recordTemplate)
import           Database.Relational                    (Config, Relation, defaultConfig, enableWarning, nameConfig,
                                                         recordConfig, relationalQuery_, untypeQuery,
                                                         verboseAsCompilerWarning)
import qualified Database.Relational.TH                 as Relational
import           Language.SQL.Keyword                   (Keyword)

import           Database.Schema.PostgreSQL.Pure.Driver (Driver, driverConfig, emptyLogChan, foldLog, getFields,
                                                         getPrimaryKey, takeLogs)

import           Data.Tuple.Homotuple                   (IsHomolisttuple, IsHomotupleItem)
import           Data.Tuple.List                        (Length)
import           GHC.TypeLits                           (KnownNat)

-- | Generate all persistable templates against defined record like type constructor.
makeRelationalRecord' :: Config
                      -> Name    -- ^ Type constructor name
                      -> Q [Dec] -- ^ Result declaration
makeRelationalRecord' config recTypeName = do
  rr <- Relational.makeRelationalRecordDefault' config recTypeName
  (((typeCon, avs), _), _) <- reifyRecordType recTypeName
  ps <- defineSqlPersistableInstances [t| SqlValue |] typeCon avs
  return $ rr ++ ps

-- | Generate all persistable templates against defined record like type constructor.
makeRelationalRecord :: Name    -- ^ Type constructor name
                     -> Q [Dec] -- ^ Result declaration
makeRelationalRecord = makeRelationalRecord' defaultConfig

-- | Generate all HDBC templates about table except for constraint keys.
defineTableDefault' :: Config            -- ^ Configuration to generate query with
                    -> String            -- ^ Schema name
                    -> String            -- ^ Table name
                    -> [(String, TypeQ)] -- ^ List of column name and type
                    -> [Name]            -- ^ Derivings
                    -> Q [Dec]           -- ^ Result declaration
defineTableDefault' config schema table columns derives = do
  modelD <- Relational.defineTableTypesAndRecord config schema table columns derives
  sqlvD <- defineSqlPersistableInstances [t| SqlValue |]
           (fst $ recordTemplate (recordConfig $ nameConfig config) schema table)
           []
  return $ modelD ++ sqlvD

-- | Generate all HDBC templates about table.
defineTableDefault :: Config            -- ^ Configuration to generate query with
                   -> String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ List of column name and type
                   -> [Name]            -- ^ Derivings
                   -> [Int]             -- ^ Indexes to represent primary key
                   -> Maybe Int         -- ^ Index of not-null key
                   -> Q [Dec]           -- ^ Result declaration
defineTableDefault config schema table columns derives primary notNull = do
  modelD <- Relational.defineTable config schema table columns derives primary notNull
  return modelD

tableAlongWithSchema :: IO Connection     -- ^ Connect action to system catalog database
                     -> Driver            -- ^ Driver definition
                     -> String            -- ^ Schema name
                     -> String            -- ^ Table name
                     -> [(String, TypeQ)] -- ^ Additional column-name and column-type mapping to overwrite default
                     -> [Name]            -- ^ Derivings
                     -> Q [Dec]           -- ^ Result declaration
tableAlongWithSchema connect drv scm tbl cmap derives = do
  let config = driverConfig drv
      getDBinfo = do
        logChan  <-  emptyLogChan
        infoP    <-  do
                       conn <- connect
                       p <-
                         (,)
                         <$> getFields drv conn logChan scm tbl
                         <*> getPrimaryKey drv conn logChan scm tbl
                       disconnect conn
                       pure p
        (,) infoP <$> takeLogs logChan

  (((cols, notNullIdxs), primaryCols), logs) <- runIO getDBinfo
  let reportWarning'
        | enableWarning config             =  reportWarning
        | otherwise                        =  const $ pure ()
      reportVerbose
        | verboseAsCompilerWarning config  =  reportWarning
        | otherwise                        =  const $ pure ()
  mapM_ (foldLog reportVerbose reportWarning' reportError) logs
  when (null primaryCols) . reportWarning'
    $ "getPrimaryKey: Primary key not found for table: " ++ scm ++ "." ++ tbl

  let colIxMap = Map.fromList $ zip [c | (c, _) <- cols] [(0 :: Int) .. ]
      ixLookups = [ (k, Map.lookup k colIxMap) | k <- primaryCols ]
      warnLk k = maybe
                 (reportWarning $ "defineTableFromDB: fail to find index of pkey - " ++ k ++ ". Something wrong!!")
                 (const $ return ())
      primaryIxs = fromMaybe [] . sequence $ map snd ixLookups
  mapM_ (uncurry warnLk) ixLookups

  let liftMaybe tyQ sty = do
        ty <- tyQ
        case ty of
          (AppT (ConT n) _) | n == ''Maybe -> [t| Maybe $(sty) |]
          _                                -> sty
      cols1 = [ (,) cn . maybe ty (liftMaybe ty) . Map.lookup cn $ Map.fromList cmap | (cn, ty) <- cols ]
  defineTableDefault config scm tbl cols1 derives primaryIxs (listToMaybe notNullIdxs)

-- | Generate all HDBC templates using system catalog informations with specified config.
defineTableFromDB' :: IO Connection     -- ^ Connect action to system catalog database
                   -> Driver            -- ^ Driver definition
                   -> String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ Additional column-name and column-type mapping to overwrite default
                   -> [Name]            -- ^ Derivings
                   -> Q [Dec]           -- ^ Result declaration
defineTableFromDB' = tableAlongWithSchema

-- | Generate all HDBC templates using system catalog informations.
defineTableFromDB :: IO Connection -- ^ Connect action to system catalog database
                  -> Driver        -- ^ Driver definition
                  -> String        -- ^ Schema name
                  -> String        -- ^ Table name
                  -> [Name]        -- ^ Derivings
                  -> Q [Dec]       -- ^ Result declaration
defineTableFromDB connect driver scm tbl = tableAlongWithSchema connect driver scm tbl []

-- | Verify composed 'Query' and inline it in compile type.
inlineVerifiedQuery :: forall p r.
                       ( KnownNat (Length p)
                       , KnownNat (Length r)
                       , IsHomotupleItem (Length p) Oid
                       , IsHomotupleItem (Length r) Oid
                       , IsHomotupleItem (Length r) ColumnInfo
                       , IsHomolisttuple (Length p) Oid
                       , IsHomolisttuple (Length r) Oid
                       , IsHomolisttuple (Length r) ColumnInfo
                       )
                    => IO Connection -- ^ Connect action to system catalog database
                    -> Name          -- ^ Top-level variable name which has 'Relation' type
                    -> Relation p r  -- ^ Object which has 'Relation' type
                    -> Config        -- ^ Configuration to generate SQL
                    -> [Keyword]     -- ^ suffix SQL words. for example, `[FOR, UPDATE]`, `[FETCH, FIRST, "3", ROWS, ONLY]` ...
                    -> String        -- ^ Variable name to define as inlined query
                    -> Q [Dec]       -- ^ Result declarations
#if MIN_VERSION_relational_query(0,13,0)
inlineVerifiedQuery connect relVar rel config sufs declName =
  Relational.inlineQuery_ check relVar rel config sufs declName
  where
    check sql = do
      when (verboseAsCompilerWarning config) . reportWarning $ "Verify with prepare: " ++ sql
      void . runIO $ do
        conn <- connect
        let psProc = parse @(Length p) @(Length r) "" (fromString sql) Nothing
        void $ sync conn psProc
        disconnect conn
#else
inlineVerifiedQuery connect relVar rel config sufs qns = do
  (p, r) <- Relational.reifyRelation relVar
  let sql = untypeQuery $ relationalQuery_ config rel sufs
  when (verboseAsCompilerWarning config) . reportWarning $ "Verify with prepare: " ++ sql
  void . runIO $ do
    conn <- connect
    let psProc = parse @(Length p) @(Length r) "" (fromString sql) Nothing
    void $ sync conn psProc
    disconnect conn
  Relational.unsafeInlineQuery (return p) (return r) sql (varCamelcaseName qns)
#endif
