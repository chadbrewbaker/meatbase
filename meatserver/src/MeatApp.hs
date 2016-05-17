{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MeatApp  where
import EventRecords
import Model
import Data.Aeson (toJSON)
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL
import Database.Persist (Entity,entityVal)
import qualified Database.Persist.Sqlite as Sqlite
import qualified Web.Scotty as S
import qualified Data.Time.Format as DTF
import Data.Time
import Database.Persist.Sql (SqlBackend)
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List as L
import qualified Data.MultiSet as MS
import qualified Data.MultiMap as MM
import Data.Pool (Pool)

parseTimestamp :: String -> UTCTime
parseTimestamp = DTF.parseTimeOrError False defaultTimeLocale "%FT%T%QZ"

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

instance Ord MeatDateCount where
    compare (a, _) (b, _)
          | a == b = EQ
          | a < b = LT
          | otherwise = GT

data MeatDate = MeatDate (Int, Int, Int)
   deriving (Eq, Ord, Show)
type MeatDateCount = (MeatDate, Int)

meatStreaks :: [Entity MeatEvent] -> [[(MS.Occur, (Int, Int, Int))]]
meatStreaks [] = []
meatStreaks xs =  streaks $ map revTup $ countingTup $ map toMeatDate xs
                where
                 toMeatDate e =  toYearMonthDay $ meatEventTimestamp $ entityVal e
                 countingTup = MS.toOccurList . MS.fromList
                 revTup (a,b) = (b,a)
                 streaks = L.groupBy (\a b -> fst a < fst b)

fromEntityMeatEvent :: Entity MeatEvent -> UTCTime
fromEntityMeatEvent e = meatEventTimestamp $ entityVal e

toYearMonthDay :: UTCTime -> (Int, Int, Int) -- (Year, Month, Day)
toYearMonthDay utime = toInts $ toTrip utime
                   where
                   toTrip t = toGregorian (utctDay t)
                   toInts (y,m,d) = (fromInteger y, m, d)

freqDays :: [UTCTime] -> [((Int, Int), Int)]
freqDays xs =  map (\x -> (fst x, snd $ maximum $ snd x)) $ monthListTups xs
         where
         countingTup ys = MS.toOccurList .  MS.fromList $ map toYearMonthDay ys
         toMMTup ((y,m,d),count) = ((y,m), (count,d))
         monthTups x = map toMMTup $  countingTup x
         monthListTups x = MM.assocs$MM.fromList $ monthTups x

freqDay :: [Entity MeatEvent] -> [((Int, Int), Int)]
freqDay xs = freqDays $ map fromEntityMeatEvent xs

helloRoute :: S.ScottyM ()
helloRoute = S.get "/" $ S.text "hello"

showDefaultRoute :: S.ScottyM ()
showDefaultRoute = S.get "/default" $ do
      now <- liftIO getDefaultCSV
      S.json $ map show now

resetDBRoute :: Pool SqlBackend -> S.ScottyM ()
resetDBRoute pool = S.get "/reset" $ do
      _ <- liftIO (cleanDB pool)
      ys <- liftIO (getMeatEvent pool)
      S.text $ TL.pack $ show $ length ys

getPeopleRoute :: Pool SqlBackend -> S.ScottyM ()
getPeopleRoute pool = S.get "/people" $ do
          ps <- liftIO (getPerson pool)
          S.json ps

getMeatEventsRoute :: Pool SqlBackend -> S.ScottyM ()
getMeatEventsRoute pool = S.get "/consumptions" $ do
          xs <- liftIO (getMeatEvent pool)
          S.json xs

getStreaksRoute :: Pool SqlBackend -> S.ScottyM ()
getStreaksRoute pool = S.get "/streaks" $ do
          xs <- liftIO (getMeatEvent pool)
          S.json $ toJSON $ meatStreaks xs

getMeatTypesRoute :: Pool SqlBackend -> S.ScottyM ()
getMeatTypesRoute pool = S.get "/meattypes" $ do
          xs <- liftIO (getMeatType pool)
          S.json xs

loadDefaultEvents :: Pool SqlBackend -> S.ScottyM ()
loadDefaultEvents pool = S.get "/loadevents" $ do
          xs <- liftIO getDefaultCSV
          liftIO $ runDB pool $ mapM_ insertMeatEvent xs
          ys <- liftIO (getMeatEvent pool)
          S.text $ TL.pack $ show $ length ys

freqDayRoute :: Pool SqlBackend -> S.ScottyM ()
freqDayRoute pool = S.get "/freqday" $ do
    xs <- liftIO (getMeatEvent pool)
    let fd = freqDay xs
    S.json fd

postJSONRoute :: Pool SqlBackend -> S.ScottyM ()
postJSONRoute pool = S.post "/upload" $ do
     e <- S.jsonData :: S.ActionM Event
     liftIO $ runDB pool $ mapM_ insertMeatEvent [e]
     ys <- liftIO (getMeatEvent pool)
     S.text $ TL.pack $ show $ length ys

allRoutes pool = do
  S.middleware logStdoutDev
  helloRoute
  showDefaultRoute
  resetDBRoute pool
  getPeopleRoute pool
  getMeatTypesRoute pool
  getMeatEventsRoute pool
  loadDefaultEvents pool
  getStreaksRoute pool
  freqDayRoute pool
  postJSONRoute pool

runApp :: IO ()
runApp = runStderrLoggingT $ Sqlite.withSqlitePool "meat.db" 10 $ \pool -> liftIO $
          runDB pool $ liftIO $ do
            mkMeatDB pool
            S.scotty 8080 $ do
              S.middleware logStdoutDev
              allRoutes pool
