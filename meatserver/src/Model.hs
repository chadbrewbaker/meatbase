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

module Model where


import qualified Data.Time.Format as DTF
import Data.Time
import Data.Time.Clock.POSIX
import Database.Persist.Sqlite
import Database.Persist.TH
-- import ClassyPrelude.Yesod excluding (.)
import Database.Persist.Quasi
import Control.Monad.Logger
import Control.Monad.Trans.Resource -- (runResourceT, ResourceT)
import Data.Pool (Pool)
import Control.Monad.IO.Class (liftIO)
import EventRecords
import Data.Traversable (mapM)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name String
    UniqueName name
    deriving Show
MeatType json
    mtype String
    UniqueType mtype
    deriving Show
MeatEvent json
    person PersonId
    meatType MeatTypeId
    timestamp UTCTime
    deriving Show

    |]

insertPerson name = do
    f <- insertBy $ Person name
    case f of
        Left (Entity uid _) -> return uid
        Right uid -> return uid


insertMeatType mtype = do
            f <- insertBy $ MeatType mtype
            case f of
                Left (Entity uid _) -> return uid
                Right uid -> return uid

insertMeatEvent e = do
                  i1 <- insertPerson (name e)
                  i2 <- insertMeatType (meat e)
                  f <- insert $ MeatEvent i1 i2 (timestamp e)
                  return f
                  -- case f of
                  --    Left (Entity uid _) -> return uid
                  --    Right uid -> return uid

insertMeatEvent' pool e = runSqlPool $ insertMeatEvent e


-- insertMeatEvent' pool e  = runSqlPool $ do
--                   x <- insertMeatEvent e
--                   return x

runDb = runStdoutLoggingT . runResourceT . withSqliteConn "meat.db" . runSqlConn

runDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

readPeople :: IO [Entity Person]
readPeople = (runDb $ selectList [] [])


getPerson :: Pool SqlBackend -> IO [Entity Person]
getPerson = runSqlPool $ selectList [] []

getMeatType :: Pool SqlBackend -> IO [Entity MeatType]
getMeatType = runSqlPool $ selectList [] []

getMeatEvent :: Pool SqlBackend -> IO [Entity MeatEvent]
getMeatEvent = runSqlPool $ selectList [] []


mkMeatDB :: Pool SqlBackend -> IO ()
mkMeatDB = runSqlPool $ do
    printMigration migrateAll
    --runMigration migrateAll

    pid <- insertPerson $ "Greg"
    liftIO $ print pid
    -- peopleIds <- insertUnique $ Person "Janice"
    -- liftIO $ print peopleIds
    -- peopleIds <- insertUnique $ Person "Carl"
    -- liftIO $ print peopleIds


    mtid <- insertMeatType $ "Deer"
    liftIO $ print mtid
    -- meatId2 <- insertUnique $ MeatType "Beef"
    -- liftIO $ print meatId2
    -- meatId3 <- insertUnique $ MeatType "Chicken"
    -- liftIO $ print meatId3
    now <- liftIO $ getCurrentTime
    -- i1 <- getByValue $ PersonName "Carl"
    -- i2 <- getByValue $ MeatType "Beef"
  --  meatEventIds <- insertUnique $ MeatEvent pid mtid now
  --  liftIO $ print meatEventIds
    pid <- insertPerson "Ralph"
    mid <- insertMeatType "DeerJerky"
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-03T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-04T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-04T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
    _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-06T05:15:25.123Z")
    return ()


-- addEvent pool e =  liftIO $ runDB pool $ do
--       id <- insertMeatEvent (name e) (meat e) (timestamp e)
--       return id
--
-- -- addEvents :: [Event] -> Pool SqlBackend -> IO ()
-- addEvents es pool = do
--        mapM es $ addEvent pool
--        return ()





--personToString :: Entity Person -> String
personToString = map (personName . entityVal)

readMeatEvents :: IO [Entity MeatEvent]
readMeatEvents = (runDb $ selectList [] [Asc MeatEventTimestamp])

cleanDB :: SqlPersistT IO ()
cleanDB = do
     deleteWhere ([] :: [Filter Person])
     deleteWhere ([] :: [Filter MeatType])
     deleteWhere ([] :: [Filter MeatEvent])

singleRecordsDB :: SqlPersistT IO ()
singleRecordsDB = do
      cleanDB
      pid <- insert $ Person "Ralph"
      mid <- insert $ MeatType "DeerJerky"
      _ <- insert $ MeatEvent pid mid (read "2014-11-30 05:15:25.123")
      return ()

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

parseTimestamp :: String -> UTCTime
parseTimestamp t = DTF.parseTimeOrError True defaultTimeLocale "%FT%T%QZ" t
  -- f <- parseField s
  -- case parseTimeorErrorTrue  defaultTimeLocale "%FT%T%QZ" s of
  --   Nothing -> fail "Unable to parse UTC time"
  --   Just g  -> return g

-- streakRecordsDB :: SqlPersistT IO ()
streakRecordsDB :: Pool SqlBackend ->  SqlPersistT IO ()
streakRecordsDB = runSqlPool $ do
      pid <- insertPerson "Ralph"
      mid <- insertMeatType "DeerJerky"
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-02T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-04T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-04T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-05T05:15:25.123Z")
      _ <- insert $ MeatEvent pid mid (parseTimestamp "2014-11-06T05:15:25.123Z")
      return ()
