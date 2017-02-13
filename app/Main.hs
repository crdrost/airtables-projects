{-# LANGUAGE OverloadedStrings #-}

module Main  where

import Airtable.Query
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

creds :: AirtableOptions
creds = AirtableOptions "keypGWoHSlUbWVS6Z" "apprFH3kxVr5DVE2U" 0

-- Analogue of Python's dict.get(self, key, defaultVal)
getWithDefault :: FromJSON j => Object -> Text -> j -> Parser j
getWithDefault obj key def = maybe def id <$> obj .:? key 


data Thread = Thread {
    tName :: Text,
    tContainedIn :: [RecordID],
    tContainments :: [RecordID],
    tDescription :: Text,
    tArchived :: Bool,
    tStatus :: Text
} deriving (Show)

instance FromJSON Thread where
    parseJSON (Object f) = Thread <$> getWithDefault f "Thread name" ""
                                  <*> f .: "Contained in"
                                  <*> f .: "Containments (raw)"
                                  <*> getWithDefault f "Description" ""
                                  <*> getWithDefault f "Archived?" False
                                  <*> getWithDefault f "Status" ""
    parseJSON _ = fail "Threads must be JSON objects."


data Containment = Containment {
    cLabel :: Text,
    container :: RecordID,
    containee :: RecordID,
    completion :: Double
} deriving (Show)

-- Question: The existing schema has the schema possibility of a Thread or Subthread containing
-- zero or multiple elements; I am assuming that they associate exactly one Thread with one 
-- Subthread. Is this correct?
instance FromJSON Containment where
    parseJSON (Object f) = Containment <$> getWithDefault f "Containment label" ""
                                       <*> fmap head (f .: "Thread")
                                       <*> fmap head (f .: "Subthread")
                                       <*> getWithDefault f "P(needed)" 100
    parseJSON _ = fail "Containments must be JSON objects."

tFilter :: (v -> Bool) -> Table v -> Table v
tFilter pred = flip deleteWhere $ const (not . pred)

someFunc = putStrLn "Hello, Stack!"

activeNodes :: Table Thread -> Table Thread
activeNodes = tFilter tArchived

rootNodes :: Table Thread -> Table Thread
rootNodes = tFilter (null . tContainedIn)

getThreads :: IO (Table Thread)
getThreads = getTable creds "Threads"

getContainments :: IO (Table Containment)
getContainments = getTable creds "Containments"

main :: IO ()
main = someFunc

