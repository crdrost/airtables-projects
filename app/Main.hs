{-# LANGUAGE OverloadedStrings #-}

module Main  where

import Airtable.Query
import Data.Aeson

creds :: AirtableOptions
creds = AirtableOptions "keypGWoHSlUbWVS6Z" "apprFH3kxVr5DVE2U" 0

data Thread = Thread {
    tName :: String,
    tContainedIn :: [String],
    tContainments :: [String],
    tDescription :: String,
    tArchived :: Bool,
    tStatus :: String
} deriving (Show)


getWithDefault obj key def = maybe def id <$> obj .:? key 

instance FromJSON Thread where
    parseJSON (Object f) = Thread <$> getWithDefault f "Thread name" ""
                                  <*> f .: "Contained in"
                                  <*> f .: "Containments (raw)"
                                  <*> getWithDefault f "Description" ""
                                  <*> getWithDefault f "Archived?" False
                                  <*> getWithDefault f "Status" ""


someFunc = putStrLn "Hello, Stack!"


main :: IO ()
main = someFunc

