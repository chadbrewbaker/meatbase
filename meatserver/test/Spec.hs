{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import MeatApp
import Model
import EventRecords
import Test.Hspec
import Test.Hspec.Wai
import qualified  Web.Scotty as S
import Data.Aeson (encode)
import Test.Hspec.Expectations ()
import qualified Database.Persist.Sqlite as Sqlite
import Control.Monad.Logger

main :: IO ()
main = runNoLoggingT $ Sqlite.withSqlitePool "meat.db" 10 $ \pool -> liftIO $ do
   let testApp = allRoutes pool
   hspec $ with (S.scottyApp testApp)  $ do

     describe "Given I clean the DB" $
                it "Then it is empty" $ do
                       _ <- liftIO (cleanDB pool)
                       get "/people" `shouldRespondWith` "[]" {matchStatus = 200}

     describe "Given I load the default CSV" $
           it "Then I dont get an error" $
              get "/loadevents" `shouldRespondWith` "25" {matchStatus = 200}

     describe "Given I ask for people after loading CSV" $
            it "Then I get Ashton, Bob, and Chuck" $
               get "/people" `shouldRespondWith`
                 "[{\"name\":\"ashton\",\"id\":1},{\"name\":\"bob\",\"id\":2},{\"name\":\"chuck\",\"id\":3}]"
                     {matchStatus = 200}

     describe "Given I ask for streaks" $
            it "Then I dont get an error" $
              get "/streaks" `shouldRespondWith` 200

     describe "Given I ask for freqs" $
            it "Then I dont get an error" $
                get   "/freqday" `shouldRespondWith`
                 "[[[2015,1],7],[[2015,2],1],[[2015,3],1],[[2015,4],1],[[2015,5],1],[[2015,8],1]]" {matchStatus = 200}

     describe "Given I ask for upload" $
            it "Then I now have 26 events" $   do
                let jenny = Event { name = "Elmer", meat = "rabbit", timestamp = Model.parseTimestamp "2014-11-02T05:15:25.123Z" }
                post "/upload" ( encode  jenny) `shouldRespondWith` "26" {matchStatus = 200}
