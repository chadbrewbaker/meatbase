{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ViewPatterns                #-}
{-# LANGUAGE DeriveTraversable           #-}
{-# LANGUAGE DeriveFoldable              #-}
{-# LANGUAGE DeriveFunctor               #-}

module EventRecords where
import Data.Time 
-- import System.Locale
import qualified Data.Time.Format as DTF

import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding (decode, decodeByName) 
import Data.Csv.Streaming
import qualified Data.Vector as V
import Control.Applicative

import Data.Traversable ( Traversable )
import qualified Data.Traversable as Tr
import Data.Foldable ( Foldable )
import qualified Control.Lens as L

data Event = Event
    { name :: !String
     ,meat :: !String
     ,timestamp :: String
    } deriving Show


instance FromNamedRecord Event where
    parseNamedRecord r = Event <$> r .: "person" <*> r .: "meat-bar-type"  <*> r .: "date"  

getStuff :: IO [Event]
getStuff  = do
  csvData <-  BL.readFile "data.csv"
  return $ case decodeByName  csvData of
    Left e -> error $ "data.csv " ++ show e
    Right (_,x) -> L.toListOf Tr.traverse x
