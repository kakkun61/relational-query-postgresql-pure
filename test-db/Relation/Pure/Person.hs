{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Relation.Pure.Person where

import           DataSource.Pure                        (connect)

import           Prelude                                (Maybe (Just, Nothing), Show (show), fail, length, sequence,
                                                         ($), (<$>), (<*>), (<>))

import           Database.PostgreSQL.Pure               (FromRecord (fromRecord), Length, ToField (toField),
                                                         ToRecord (toRecord))
import           Database.PostgreSQL.Pure.Parser        (column)
import           Database.Relational.PostgreSQL.Pure.TH (defineTableFromDB)
import           Database.Schema.PostgreSQL.Pure        (driver)
import           GHC.Generics                           (Generic)

defineTableFromDB connect driver "public" "person" [''Show, ''Generic]

instance FromRecord Person where
  fromRecord decode [i0, i1] = Person <$> column decode i0 <*> column decode i1
  fromRecord _ is            = fail $ "length mismatch: expected 2: actual: " <> show (length is)

instance ToRecord Person where
  toRecord backendParams encode Nothing [f0, f1] (Person v0 v1) =
    sequence
      [ toField backendParams encode Nothing f0 v0
      , toField backendParams encode Nothing f1 v1
      ]
  toRecord backendParams encode (Just [o0, o1]) [f0, f1] (Person v0 v1) =
    sequence
      [ toField backendParams encode (Just o0) f0 v0
      , toField backendParams encode (Just o1) f1 v1
      ]
  toRecord _ _ (Just os) [_] _ =
    fail $ "the number of OIDs must be 2, actually " <> show (length os)
  toRecord _ _ _ fs _ =
    fail $ "the number of format codes must be 2, actually " <> show (length fs)

type instance Length Person = 2
