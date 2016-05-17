{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module EventRecords where
import Data.Time
-- import qualified Data.Time.Format as DTF
import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding (decode, decodeByName)
import Data.Csv.Streaming
-- import qualified Data.Vector as V
-- import Control.Applicative

-- import Data.Traversable ( Traversable )
import qualified Data.Traversable as Tr
-- import Data.Foldable ( Foldable )
import qualified Control.Lens as L
-- import qualified Data.List as List
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data Event = Event
    { name :: !String
     ,meat :: !String
     ,timestamp :: UTCTime
    } deriving (Show,  Generic)

instance FromNamedRecord Event where
    parseNamedRecord r = Event <$> r .: "person" <*> r .: "meat-bar-type"  <*> r .: "date"

instance ToJSON Event
instance FromJSON Event

instance FromField UTCTime where
   parseField s = do
     f <- parseField s
     case parseTimeM True  defaultTimeLocale "%FT%T%QZ" f of
       Nothing -> fail "Unable to parse UTC time"
       Just g  -> return g

-- runs :: Ord a => [a] -> [[a]]
-- runs xs = [head xs] ++ ( foldl (++) [] $ map isRun  $ zip xs (tail xs))
--     where
--      isRun (a, b) = if (b > a) then [b] else []



getDefaultCSV :: IO [Event]
getDefaultCSV = getStuff "data.csv"

getStuff :: String -> IO [Event]
getStuff s = do
  csvData <-  BL.readFile s
  return $ case decodeByName  csvData of
    Left e -> error $ "data.csv " ++ show e
    Right (_,x) -> L.toListOf Tr.traverse x
