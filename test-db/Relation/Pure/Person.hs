{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Relation.Pure.Person where

import           DataSource.Pure                        (connect)

import           Prelude                                (Show)

import           Database.Relational.PostgreSQL.Pure.TH (defineTableFromDB)
import           Database.Schema.PostgreSQL.Pure        (driver)
import           GHC.Generics                           (Generic)

defineTableFromDB connect driver "public" "person" [''Show, ''Generic]
