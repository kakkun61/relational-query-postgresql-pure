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

type instance Length Person = 2
