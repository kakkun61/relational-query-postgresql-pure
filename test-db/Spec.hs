import           Database.HDBC.Record                      (runQuery)
import           Database.HDBC.Session                     (handleSqlError', withConnectionIO)
import           Database.PostgreSQL.Pure                  as Pure
import           Database.Relational                       (relationalQuery)
import           Database.Relational.PostgreSQL.Pure.Query as Pure
import           Test.Hspec

import qualified DataSource                                as DS
import qualified DataSource.Pure                           as DSP
import qualified Relation.Person                           as Person
import qualified Relation.Pure.Person                      as PersonPure

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
  it "run" $ do
    handleSqlError' $ withConnectionIO DS.connect $ \conn -> do
      connPure <- DSP.connect
      persons <- ((\(Person.Person id name) -> (id, name)) <$>) <$> runQuery conn (relationalQuery Person.person) ()
      personsPure <- ((\(PersonPure.Person id name) -> (id, name)) <$>) <$> Pure.runQuery' connPure (relationalQuery PersonPure.person) ()
      Pure.disconnect connPure
      personsPure `shouldBe` persons
