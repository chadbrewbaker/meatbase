{-# OPTIONS_GHC -Wall #-}
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

import EventRecords
import Data.Time
import qualified Data.Time.Format as DTF
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Pool (Pool)

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
                  insert $ MeatEvent i1 i2 (timestamp e)

insertMeatEvent' pool e = runSqlPool $ insertMeatEvent e

runDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
runDB = flip runSqlPool

getPerson :: Pool SqlBackend -> IO [Entity Person]
getPerson = runSqlPool $ selectList [] []

getMeatType :: Pool SqlBackend -> IO [Entity MeatType]
getMeatType = runSqlPool $ selectList [] []

getMeatEvent :: Pool SqlBackend -> IO [Entity MeatEvent]
getMeatEvent = runSqlPool $ selectList [] []


mkMeatDB :: Pool SqlBackend -> IO ()
mkMeatDB = runSqlPool $ do
     printMigration migrateAll
     return ()

cleanDB :: Pool SqlBackend -> IO ()
cleanDB = runSqlPool $ do
     deleteWhere ([] :: [Filter MeatEvent])
     deleteWhere ([] :: [Filter Person])
     deleteWhere ([] :: [Filter MeatType])
     return ()

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

parseTimestamp :: String -> UTCTime
parseTimestamp = DTF.parseTimeOrError True defaultTimeLocale "%FT%T%QZ"

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
