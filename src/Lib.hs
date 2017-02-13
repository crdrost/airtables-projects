{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Airtable.Query
import Data.Aeson

creds :: AirtableOptions
creds = AirtableOptions "keypGWoHSlUbWVS6Z" "apprFH3kxVr5DVE2U" 0

data Thread = Thread {tName :: String, tContainedIn :: [String], tContainments :: [String], tDescription :: String}

instance FromJSON Thread where
    parseJSON (Object o) = Thread <$> o .: "Thread name"
                                  <*> o .: "Contained In"
                                  <*> o .: "Containments (raw)"
                                  <*> o .: "Description"



-- remove this at some point --
someFunc :: IO ()
someFunc = putStrLn "someFunc"


