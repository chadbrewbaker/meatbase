{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Example (runApp,  app) where
import EventRecords
import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import qualified Data.Text as T
import Database.Persist
import qualified Database.Persist.Sqlite as Sqlite

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Web.Scotty as S
import qualified Data.Time.Format as DTF
import Data.Time
import Database.Persist.Sql
import qualified Database.Sqlite  as S
import Model
import Control.Monad.IO.Class
import Control.Monad.Logger -- (runNoLoggingT)


import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Data.List as L
import qualified Data.MultiSet as MS
import qualified Data.MultiMap as MM -- hiding (map)
import Data.Pool (Pool)
-- myGlobalPool :: IORef ConnectionPool
-- {-# NOINLINE myGlobalVar #-}
-- myGlobalPool = unsafePerformIO (newIORef 17)


parseTimestamp :: String -> UTCTime
parseTimestamp t = DTF.parseTimeOrError False defaultTimeLocale "%FT%T%QZ" t

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

-- freqDayMap :: [Entity MeatEvent] -> map Int Int
-- freqDayMap = undefined

streaks :: [Entity MeatEvent] -> [ [(UTCTime,Int)] ]
streaks = undefined

consumption = undefined

toYearMonthDay :: UTCTime -> (Int, Int, Int) -- (Year, Month, Day)
toYearMonthDay utime = toInts $ toTrip utime
                   where
                   toTrip t = toGregorian (utctDay t)
                   toInts (y,m,d) = (fromInteger y, m, d)

-- maxRep x = L.last $ L.sort $ countRep x
-- countRep s =  map (\x->(length x, [head x])) . L.group . L.sort s

-- [timestamp] -> [(year,month, mostFreqDay)]
freqDays :: [(UTCTime)] -> [((Int, Int), Int)]
freqDays xs =  map (\x -> (fst x, snd $ L.last $ L.sort $ snd x)) $ monthListTups xs
         where
         countingTup ys = MS.toOccurList .  MS.fromList $ map toYearMonthDay ys
         toMMTup ((y,m,d),count) = ((y,m), (count,d))
         monthTups x = map toMMTup $  countingTup x
         monthListTups x = MM.assocs$MM.fromList $ monthTups x
        -- toFreq ys  =   map( (fst, last.sort.snd) ) ys
       --  freqDay (a, ys) = (a, snd$last $ sort ys)


freqDay ::  [Entity MeatEvent] -> [(Int, Int, Int)]
freqDay = undefined
--freqDay = freqDays $ getTimestamps
-- grab the stream of meat events
-- yank out the timestamp to a list
--


-- app' :: Connection ->  S.ScottyM ()

--app conn = do
app' = do
  S.middleware logStdoutDev
  S.get "/" $ S.text "hello"
  S.get "/default" $ do
        now <- liftIO getDefaultCSV
        S.json$ map show $ now
  S.get "/people" $ do
         people <- liftIO $ readPeople
         S.json $ people

  S.get "/streaks" $ S.text "[]"
  S.get "/consumption" $ S.text "[]"
  S.get "/freqday" $ do
        events <- liftIO $ readMeatEvents
        S.json $ events

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

  S.post "/upload" $ do
    fs <- S.files
    -- https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Char8.html
    -- http://stackoverflow.com/questions/7815402/convert-a-lazy-bytestring-to-a-strict-bytestring
    -- let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
    S.text $ "I got a file"
  S.get "/addmike" $ do
      mid <- liftIO $ runDb $ insertPerson "Mikey"
     -- let mic26 = Person "Michael"
     --  mid <- liftIO $ runDb $ insert mic26
      let jerky = MeatType "DeerJerky"
      djid <- liftIO $ runDb $ insert jerky
      now <- liftIO $ getCurrentTime
      foo <- liftIO $ runDb $ insert (MeatEvent mid djid now)
      results <- liftIO $ readPeople

      S.json $ map entityIdToJSON $ results
      -- S.text $ readPeople $ results

-- readPerson :: IO [Sqlite.Entity Person]
-- readPerson =   runDb $ Sqlite.selectList [] []

app :: IO Application
app = S.scottyApp app'


-- runDb :: SqlPersist (ResourceT IO) a -> IO a
-- runDb query = runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn $ query

-- readPosts :: IO [Entity Post]
-- readPosts = (runDb $ selectList [] [LimitTo 10])

-- blaze = S.html . renderHtml

-- main = do
--   runDb $ runMigration migrateAll



--runDb :: SqlPersist (ResourceT IO) a -> IO a
-- runDb query = runResourceT . Sqlite.withSqliteConn ":memory:" . runSqlConn $ query


-- runDb :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
-- runDb = runNoLoggingT
--       . runResourceT
--       . Sqlite.withSqliteConn ":memory:"
--       . runSqlConn

main :: IO ()
main = runApp

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistPool = PersistPool ConnectionPool
openConnectionCount :: Int
openConnectionCount = 1




runApp :: IO ()
runApp = do

    -- runDb $ do
    --   printMigration migrateAll
    --   liftIO $ S.scotty 8080 app'

       runStderrLoggingT $ Sqlite.withSqlitePool "meat.db" 10 $ \pool -> liftIO $ do
          runDB pool $ liftIO $ do
            mkMeatDB pool
            S.scotty 8080 $ do

              S.middleware logStdoutDev
              S.get "/" $ S.text "hello"
              S.get "/default" $ do
                    now <- liftIO getDefaultCSV
                    S.json$ map show $ now
              S.get "/people" $ do
                  ps <- liftIO (getPerson pool)
                  S.json ps
              S.get "/meattypes" $ do
                      xs <- liftIO (getMeatType pool)
                      S.json xs
              S.get "/meatevents" $ do
                      xs <- liftIO (getMeatEvent pool)
                      S.json xs
              S.get "/loadevents" $ do
                        xs <- liftIO getDefaultCSV
                        liftIO $ runDB pool $ mapM_ insertMeatEvent xs
                      --  liftIO $ mapM_ ( insertMeatEvent' pool ) xs

                        -- doInsertMeatEvent
                          --where
                            --doInsertMeatEvent e = liftIO $ insertMeatEvent (name e) (meat e) (timestamp e)
                        ys <- liftIO (getMeatEvent pool)
                        S.json ys

              -- middleware logStdout
              -- middleware $ gzip $ def { gzipFiles = GzipCompress }
              -- middleware $ staticPolicy (noDots >-> addBase "Static")
              -- home
              -- movies pool
              -- login
