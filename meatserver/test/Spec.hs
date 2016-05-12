{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson (Value(..), object, (.=))
import           Data.Time
import           Example (app)
import           Model
import Test.Hspec.Expectations ()
import Database.Persist hiding(get)
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.Text as Text


--import Application           (makeFoundation, makeLogWare)
--import ClassyPrelude         as X
--import Database.Persist      hiding (get)
--import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
--import Foundation            as X
--import Model                 as X
--import Test.Hspec            as X
--import Text.Shakespeare.Text (st)
--import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
--import Yesod.Test            as X



main :: IO ()
main = hspec $ do
     describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
           head [23 ..] `shouldBe` (23 :: Int) 

     describe "Given I ask for the year" $ do
       it "Then I get 2016" $ do
          now <- getCurrentTime
          let today = utctDay now
          let (year, month, day) = toGregorian today
          (fromInteger year) `shouldBe` (2016 :: Int)


     spec

-- readTodos :: IO [Sqlite.Entity Person]
-- readTodos =  runDb $ Sqlite.selectList [] []

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with 'hello'" $ do
      get "/" `shouldRespondWith` "hello"

    it "responds with 200 / 'hello'" $ do
      get "/" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  -- describe "GET /some-json" $ do
  --   it "responds with some JSON" $ do
  --     get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]

  describe "GET /people" $ do
    it "responds with 200 and people list" $ do
      get "/people" `shouldRespondWith` "[]" {matchStatus = 200}

  describe "GET /streaks" $ do
    it "responds with 200 and people list" $ do
      get "/streaks" `shouldRespondWith` "[]" {matchStatus = 200}

  describe "GET /freqday" $ do
    it "responds with 200 and largest day" $ do
      get "/freqday" `shouldRespondWith` "1" {matchStatus = 200}


  describe "read in data from csv" $ do
     it "does stuff" $ do
       get "/default" `shouldRespondWith` "1" {matchStatus = 404}
