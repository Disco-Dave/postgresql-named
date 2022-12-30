module Database.PostgreSQL.Simple.FromRow.NamedSpec (spec) where

import qualified CatDb
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import Data.Time (Day)
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.FromRow.Named (fieldByName)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


data Cat = Cat
  { catId :: Integer
  , catName :: Text
  , catColor :: Text
  , catPersonality :: Text
  , catDateOfBirth :: Day
  }
  deriving (Show, Eq)


queryByName :: Pool Postgres.Connection -> Text -> IO (Maybe Cat)
queryByName pool name = do
  let query =
        [sql|
          SELECT
            c.id AS cat_id
            ,c.name
            ,c.date_of_birth
            ,c.personality
            ,c.color
            ,c.created_at
          FROM public.cats AS c
          WHERE c.name = ?;
        |]

      params =
        Postgres.Only name

      rowParser = do
        catId <- fieldByName "cat_id"
        catName <- fieldByName "name"
        catColor <- fieldByName "color"
        catPersonality <- fieldByName "personality"
        catDateOfBirth <- fieldByName "date_of_birth"
        pure Cat{..}

  rows <-
    Pool.withResource pool $ \connection ->
      Postgres.queryWith rowParser connection query params

  pure $ listToMaybe rows


spec :: Spec
spec =
  aroundAll CatDb.withPool $ do
    describe "fieldByName" $ do
      it "Waymond" $ \pool -> do
        Just Cat{..} <- queryByName pool "Waymond"

        catId `shouldSatisfy` (> 0)
        catName `shouldBe` "Waymond"
        catColor `shouldBe` "grey"
        catPersonality `shouldBe` "jock"
        catDateOfBirth `shouldBe` Time.fromGregorian 2020 01 02

      it "Mochi" $ \pool -> do
        Just Cat{..} <- queryByName pool "Mochi"

        catId `shouldSatisfy` (> 0)
        catName `shouldBe` "Mochi"
        catColor `shouldBe` "orange"
        catPersonality `shouldBe` "peppy"
        catDateOfBirth `shouldBe` Time.fromGregorian 2019 02 03

      it "Sarabi" $ \pool -> do
        Just Cat{..} <- queryByName pool "Sarabi"

        catId `shouldSatisfy` (> 0)
        catName `shouldBe` "Sarabi"
        catColor `shouldBe` "white"
        catPersonality `shouldBe` "snooty"
        catDateOfBirth `shouldBe` Time.fromGregorian 2018 03 04
