{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Relational.PostgreSQL.Pure.Query
  ( runQuery'
  ) where

import qualified Data.ByteString.UTF8                    as BSU
import           Data.String                             (IsString (fromString))
import           Data.Tuple.Homotuple                    (Homotuple, IsHomolisttuple, IsHomotupleItem)
import           Data.Tuple.List                         (HasLength)
import           Database.PostgreSQL.Placeholder.Convert (convertQuestionMarkStyleToDollarSignStyle)
import           Database.PostgreSQL.Pure                (ColumnInfo, Connection, FormatCode (BinaryFormat), FromRecord,
                                                          Length, Oid, ToRecord, bind, execute, parameters, parse,
                                                          records, sync)
import qualified Database.PostgreSQL.Pure                as Pure
import           Database.Relational                     (Query, untypeQuery)
import           GHC.TypeLits                            (KnownNat)

-- | Prepare SQL, bind parameters, execute statement and strictly fetch all records.
runQuery' :: forall p r.
             ( ToRecord p
             , FromRecord r
             , KnownNat (Length p)
             , KnownNat (Length r)
             , HasLength (Homotuple (Length r) ColumnInfo)
             , IsHomotupleItem (Length p) Oid
             , IsHomotupleItem (Length r) Oid
             , IsHomotupleItem (Length r) ColumnInfo
             , IsHomolisttuple (Length p) Oid
             , IsHomolisttuple (Length r) Oid
             , IsHomolisttuple (Length r) ColumnInfo
             )
          => Connection -- ^ Database connection
          -> Query p r -- ^ Query to get record type 'a' requires parameter 'p'
          -> p -- ^ Parameter type
          -> IO [r] -- ^ Action to get records
runQuery' conn q p =
  case convertQuestionMarkStyleToDollarSignStyle $ fromString $ untypeQuery q of
    Left err -> fail err
    Right q' -> do
      let psp = parse "" (Pure.Query q') Nothing
      pp <- bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) p psp
      let ep = execute 0 (pure . BSU.toString) pp
      ((_, _, e, _), _) <- sync conn ep
      pure $ records e
