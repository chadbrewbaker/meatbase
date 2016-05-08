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

import Data.Time
import Data.Time.Clock.POSIX
import Database.Persist.Sqlite
import Database.Persist.TH
-- import ClassyPrelude.Yesod excluding (.)
import Database.Persist.Quasi
import Control.Monad.Logger
import Control.Monad.Trans.Resource -- (runResourceT, ResourceT)


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

-- runSql :: SqlPersist IO a -> IO a
-- runSql = withSqliteConn ":memory:" . runSqlConn 

-- runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
-- runDb =  runNoLoggingT . runResourceT . withSqliteConn "meat.db" . runSqlConn

runDb = runStdoutLoggingT . runResourceT . withSqliteConn "meat.db" . runSqlConn

readPeople :: IO [Entity Person]
readPeople = (runDb $ selectList [] [LimitTo 10])

--personToString :: Entity Person -> String
personToString = map (personName . entityVal)

readMeatEvents :: IO [Entity MeatEvent]
readMeatEvents = (runDb $ selectList [] [LimitTo 10])

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

streakRecordsDB :: SqlPersistT IO ()
streakRecordsDB = do
      cleanDB
      pid <- insert $ Person "Ralph"
      mid <- insert $ MeatType "DeerJerky"
      _ <- insert $ MeatEvent pid mid (read "2014-11-03 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-04 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-04 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-05 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-05 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-05 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-05 05:15:25.123")
      _ <- insert $ MeatEvent pid mid (read "2014-11-06 05:15:25.123")
      return ()

