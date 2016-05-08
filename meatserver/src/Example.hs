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


module Example (runApp, app) where
import EventRecords
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import Data.Text as T hiding(map)
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

parseTimestamp :: String -> UTCTime
parseTimestamp t = DTF.parseTimeOrError False defaultTimeLocale "%FT%T%QZ" t

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

-- freqDayMap :: [Entity MeatEvent] -> map Int Int
-- freqDayMap = undefined

streaks :: [Entity MeatEvent] -> [ [(UTCTime,Int)] ]
streaks = undefined

consumption = undefined

freqDay ::  [Entity MeatEvent] -> Int -> Int
freqDay = undefined

app' :: S.ScottyM ()
app' = do
 -- S.middleware logStdoutDev
  S.get "/" $ S.text "hello"
  S.get "/people" $ S.text "[]"
  S.get "/streaks" $ S.text "[]"
  S.get "/consumption" $ S.text "[]"
  S.get "/freqday/:month" $ do
        month <- S.param "month"
        S.text $ month

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

  S.post "/upload" $ do
    fs <- S.files
    -- https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Char8.html
    -- http://stackoverflow.com/questions/7815402/convert-a-lazy-bytestring-to-a-strict-bytestring
    -- let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
    S.text $ "I got a file"
  S.get "/addmike" $ do
     -- let mic26 = Person "Michael"
     -- micK <- liftIO $ runDb $ insert mic26
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

runApp :: IO ()
runApp = do

    -- Sqlite.runSqlite ":memory:" $ Sqlite.runMigration migrateAll
    --Sqlite.withSqlitePool ":memory:" 10 $ \pool ->
    --S.runNoLoggingT $ 
    --
    --Sqlite.withSqliteConn ":memory:" $ \conn -> 

    -- conn <- open' (T.pack ":memory:")
    -- runSqliteConn conn $ do

    --Sqlite.runSqlite ":memory:" $ Sqlite.runMigration migrateAll $ \conn ->
      -- Sqlite.runMigration migrateAll
      --runDb $ runMigration migrateAll
     
  -- liftIO $ Db.runSqlite "example.db" insertInitialData
     runDb $ do
       printMigration migrateAll
       liftIO $ S.scotty 8080 app'


     -- Sqlite.runSqlite "meat.db" $ do
     --    runMigration migrateAll
     --    liftIO $ S.scotty 8080 app'

    -- Sqlite.runSqlite ":memory:" $ runMigration migrateAll
   --  Sqlite.withSqlitePool ":memory:" 1 $ \pool ->
        


      ----runNoLoggingT $ Sqlite.withSqliteConn ":memory:" $ \conn -> do
      ----   runSqlConn (runMigration migrateAll) conn
        --  liftIO $ S.scotty 8080 app'   


       --Sqlite.runSqlite ":memory:" 
      -- S.scotty 8080 app'
    -- close' conn
--   memdb = ":memory:" 
-- filedb = "sqlite3.db"
-- main :: IO ()
     --Sqlite.runSqlite ":memory:" $ do
      --    Sqlite.runMigration migrateAll



-- open' :: T.Text -> IO SqlBackend
-- open' t = S.open t >>= flip Sqlite.wrapConnection (\_ _ _ _ -> return ()) -- no logging

-- runSqliteConn :: SqlBackend -> SqlPersistM a -> IO a
-- runSqliteConn = flip runSqlPersistM

--------------------------------------------------------------------------------
-- Example

-- main :: IO ()
-- main = do
--     conn <- open' (T.pack ":memory:")
--     runSqliteConn conn $ do
--         return () -- logic
--     close' conn
     
