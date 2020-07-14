{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Schema.PostgreSQL.Pure
  ( driver
  ) where

import           Language.Haskell.TH                               (TypeQ)

import           Control.Applicative                               ((<|>))
import           Control.Monad                                     (guard)
import           Control.Monad.Trans.Class                         (lift)
import           Control.Monad.Trans.Maybe                         (MaybeT)
import           Data.Char                                         (toLower)
import           Data.Map                                          (fromList)

import           Database.Relational.PostgreSQL.Pure.Query         (runQuery')

import           Database.Relational.Schema.PostgreSQL             (config, getType, normalizeColumn, notNull,
                                                                    primaryKeyLengthQuerySQL, primaryKeyQuerySQL)
import qualified Database.Relational.Schema.PostgreSQL             as Schema
import           Database.Relational.Schema.PostgreSQL.PgAttribute (PgAttribute (PgAttribute))
import           Database.Relational.Schema.PostgreSQL.PgType      (PgType (PgType))
import qualified Database.Relational.Schema.PostgreSQL.PgType      as Type

import           Database.Schema.PostgreSQL.Pure.Driver            (Driver, LogChan, TypeMap, driverConfig, emptyDriver,
                                                                    failWith, getFieldsWithMap, getPrimaryKey,
                                                                    hoistMaybe, maybeIO, putVerbose)

import           Data.Tuple.Homotuple.Only                         ()
import           Data.Tuple.List.Only                              ()
import           Database.PostgreSQL.Pure                          (Connection)
import           Database.PostgreSQL.Pure.Oid                      (Oid (Oid))
import           Database.Relational                               (Query)
import           GHC.Int                                           (Int16, Int32)
import           Unsafe.Coerce                                     (unsafeCoerce)


type Column = (Oid, String, Oid, Int32, Int16, Int16, Int32, Int32, Int32, Bool, Char, Char, Bool, Bool, Bool, Bool, Int32, Oid, Oid, String, Oid, Oid, Int16, Bool, Char, Char, Bool, Bool, Char, Oid, Oid, Oid, Char, Char, Bool, Oid, Int32, Int32, Oid, Maybe String)

originalColumn :: Column -> Schema.Column
originalColumn (Oid i0, i1, Oid i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, Oid i17, Oid i100, i101, Oid i102, Oid i103, i104, i105, i106, i107, i108, i109, i110, Oid i111, Oid i112, Oid i113, i114, i115, i116, Oid i117, i118, i119, Oid i120, i121) =
  ( PgAttribute i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17
  , PgType i100 i101 i102 i103 i104 i105 i106 i107 i108 i109 i110 i111 i112 i113 i114 i115 i116 i117 i118 i119 i120 i121
  )

logPrefix :: String -> String
logPrefix =  ("PostgreSQL: " ++)

putLog :: LogChan -> String -> IO ()
putLog lchan = putVerbose lchan . logPrefix

compileError :: LogChan -> String -> MaybeT IO a
compileError lchan = failWith lchan . logPrefix

getPrimaryKey' :: Connection
               -> LogChan
               -> String
               -> String
               -> IO [String]
getPrimaryKey' conn lchan scm' tbl' = do
  let scm = map toLower scm'
      tbl = map toLower tbl'
  mayKeyLen <- runQuery' conn primaryKeyLengthQuerySQL (scm, tbl)
  case mayKeyLen of
    []        ->
      return []
    [keyLen]  -> do
      primCols <- runQuery' conn (primaryKeyQuerySQL keyLen) (scm, tbl)
      let primaryKeyCols = normalizeColumn <$> primCols
      putLog lchan $ "getPrimaryKey: primary key = " ++ show primaryKeyCols
      return primaryKeyCols
    _:_:_     -> do
      putLog lchan   "getPrimaryKey: Fail to detect primary key. Something wrong."
      return []

getColumns' :: TypeMap
            -> Connection
            -> LogChan
            -> String
            -> String
            -> IO ([(String, TypeQ)], [Int])
getColumns' tmap conn lchan scm' tbl' = maybeIO ([], []) id $ do
  let scm = map toLower scm'
      tbl = map toLower tbl'
      columnQuerySQL :: Query (String, String) Column
      columnQuerySQL = unsafeCoerce Schema.columnQuerySQL
  cols <- lift $ (originalColumn <$>) <$> runQuery' conn columnQuerySQL (scm, tbl)
  guard (not $ null cols) <|>
    compileError lchan ("getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl)

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  lift . putLog lchan
    $  "getFields: num of columns = " ++ show (length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let getType' col =
        hoistMaybe (getType (fromList tmap) col) <|>
        compileError lchan ("Type mapping is not defined against PostgreSQL type: " ++ Type.typname (snd col))

  types <- mapM getType' cols
  return (types, notNullIdxs)

-- | Driver implementation
driver :: Driver
driver =
  emptyDriver { getFieldsWithMap = getColumns' }
              { getPrimaryKey    = getPrimaryKey' }
              { driverConfig     = config }
