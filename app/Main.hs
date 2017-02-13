{-# LANGUAGE OverloadedStrings #-}

module Main  where

import Airtable.Query
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree)
import System.Environment (getArgs)

-- Analogue of Python's dict.get(self, key, defaultVal)
getWithDefault :: FromJSON j => Object -> Text -> j -> Parser j
getWithDefault obj key def = maybe def id <$> obj .:? key 

-- The Thread type contained in the Airtable with some Maybes mapped to default values.
data Thread = Thread {
    tName :: !Text,
    tContainedIn :: ![RecordID],   -- note that these are indexes of the Containments below!
    tContainments :: ![RecordID],  -- likewise!
    tDescription :: !Text,
    tArchived :: !Bool,
    tStatus :: !Text
} deriving (Show, Eq, Ord)

-- How to produce that type from the raw JSON that Airtable spits out at you.
instance FromJSON Thread where
    parseJSON (Object f) = Thread <$> getWithDefault f "Thread name" ""
                                  <*> f .: "Contained in"
                                  <*> f .: "Containments (raw)"
                                  <*> getWithDefault f "Description" ""
                                  <*> getWithDefault f "Archived?" False
                                  <*> getWithDefault f "Status" ""
    parseJSON _ = fail "Threads must be JSON objects."

-- The Containment type contained in the Airtable
data Containment = Containment {
    cLabel :: !Text,
    container :: !RecordID, -- of a Thread!
    containee :: !RecordID, -- also of a Thread!
    completion :: !Double
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

-- like List.filter for Tables.
tFilter :: (v -> Bool) -> Table v -> Table v
tFilter pred = flip deleteWhere $ const (not . pred)

-- We do not care about archived Threads.
-- This might also need to be amended to exclude Threads with a blank tName?
activeNodes :: Table Thread -> Table Thread
activeNodes = tFilter (not . tArchived)

-- We start the tree from the root nodes which are not contained in anything else.
rootNodes :: Table Thread -> Table Thread
rootNodes = tFilter (null . tContainedIn)

-- GOAL: turn each Thread into an Outline. This is an abstraction for the actual XML output type,
-- notice that this is a fully recursive data structure, not going through the Containment table or 
-- Record IDs like Threads do. But this also means we'll need to watch out for cycles...
data Outline = Outline {
    oText :: String,
    oNote :: Maybe String,
    oComplete :: Maybe String,
    oChildren :: [Outline]
} deriving (Show, Eq, Ord) 

-- we're going to pass these contexts down through the walk through our Threads; the important part
-- is the subpath to detect cycles, the rest could have been done with lexical scoping...
data ThreadContext = TC {threads :: !(Table Thread), containment :: !(Table Containment), subpath :: ![RecordID]}

-- could also do this with lenses I suppose...
-- rec &. ctx  ==  "give me a new context with rec in it."
(&.) :: RecordID -> ThreadContext -> ThreadContext
rec &. (TC t c s) = TC t c (rec : s)

-- rec &? ctx  ==  "is rec in the subpath of this context?"
(&?) :: RecordID -> ThreadContext -> Bool
rec &? ctx = not . null . filter (== rec) . subpath $ ctx

-- Now for the trickier logic, we need to get all of a thread's child-threads, so long as they do 
-- not generate cycles and are in the list of active threads. We also want to sort by completion.
threadChildren :: ThreadContext -> Thread -> [(RecordID, Thread)]
threadChildren ctx = removeComp . concatMap compThread . map (select $ containment ctx) . tContainments
    where
        -- postprocess by sorting, then removing the completion that was used for sorting.
        removeComp :: [(Double, Thread, RecordID)] -> [(RecordID, Thread)]
        removeComp = map removal . List.sort where
            removal (completion, thread, record) = (record, thread)

        -- map each containment to either 0 or 1 triples, which are all concatted via concatMap.
        -- essentially filters out the Nothings while mapping the Justs to their triples.
        compThread :: Containment -> [(Double, Thread, RecordID)]
        compThread c = maybe [] (\t -> [(completion c, t, containee c)]) $ maybeThread c

        -- as part of the above: map each containment to a thread if one exists and is not a cycle.
        maybeThread :: Containment -> Maybe Thread
        maybeThread c = dropCycles (containee c) >>= selectMaybe (threads ctx)

        -- finally, check for cycles in the parents of the current path...
        dropCycles :: RecordID -> Maybe RecordID
        dropCycles record 
            | record &? ctx = Nothing     -- TODO: insert fake records to notify about cycles?
            | otherwise     = Just record

-- That's fine but it does not actually produce the recursive data structure, so here's where we
-- recurse on descendants to produce the whole Outline from a given (RecordID, Thread) pair:
outlineFromThread :: ThreadContext -> (RecordID, Thread) -> Outline
outlineFromThread ctx (record, thread) = result where
    result = Outline (unpack $ tName thread) processDescr processStatus children
    processStatus 
        | tStatus thread == "Done" = Just "true" 
        | otherwise                = Nothing
    processDescr 
        | tDescription thread /= "" = Just (unpack $ tDescription thread)
        | otherwise                 = Nothing
    children = map recurse $ threadChildren ctx' thread where
        ctx' = record &. ctx
        recurse = outlineFromThread ctx'

-- Then we need to convert outlines to/from XML:
instance XmlPickler Outline where
    xpickle = xpElem "outline" $ 
              xpWrap (\(a,b,c,d) -> Outline a b c d, \(Outline a b c d) -> (a, b, c, d)) $
              xp4Tuple (xpAttr "text" xpText)
                       (xpOption (xpAttr "_note" xpText))
                       (xpOption (xpAttr "_complete" xpText))
                       (xpList xpickle)

data OPML = OPML {version :: String, ownerEmail :: String, outlines :: [Outline] } deriving (Show)
instance XmlPickler OPML where
    xpickle = xpOPML

xpOPML = xpElem "opml" $
         xpWrap (\(a,b,c) -> OPML a b c, \(OPML a b c) -> (a, b, c)) $
         xpTriple (xpAttr "version" xpText)
                  (xpElem "head" $ xpElem "ownerEmail" xpText)
                  (xpElem "body" $ xpList xpickle)


runMain :: AirtableOptions -> IO ()
runMain creds = do
    activeThreads <- activeNodes <$> getTable creds "Threads"
    containments <- getTable creds "Containments"
    let roots = toList $ rootNodes (activeThreads)
    let ctx = TC activeThreads containments []
    let outlines = map (outlineFromThread ctx) roots
    runX (
        arr (const $ OPML "2.0" "drostie@zoho.com" outlines)
        >>> xpickleDocument xpOPML [withIndent yes] "")
    return ()

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: airtables-projects <key> <airtable>"
    putStrLn ""
    putStrLn "Prints a 'Threads' and 'Containments' database as an OPML file describing a Workflowy tree."
    putStrLn "This executable prints to stdout."

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 
       then printUsage
       else case args of [key, table] -> if take 3 key /= "key" || take 3 table /= "app" 
                                            then printUsage
                                            else runMain (AirtableOptions key table 0)

