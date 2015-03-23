{-# LANGUAGE OverloadedStrings #-}
-- reason for OverloadedStrings:
--
-- It would be very tedious to have to manually type out Name "p" Nothing Nothing
-- every time we want a paragraph. If you turn on OverloadedStrings, "p" will resolve
-- to that all by itself!
--
-- http://www.yesodweb.com/book/xml

module Main where

import Prelude (read, print, show, Show, Read, (*), (+), (-), (/), round, putStrLn)
import System.Environment  (getArgs)
import System.IO (writeFile)
import GHC.Base
import GHC.List (concat, reverse)
import Control.Monad (sequence_)
import Data.Maybe
import Data.List (maximum, minimum, find, filter, zip, foldr1)
import Data.Range.Range (Range(..), rangesOverlap)
import Data.Tuple (fst, snd)
import Data.Map (lookup)
import Text.Read (readMaybe)
import Text.XML (parseLBS_, def, Name, Document, Node(..), Element, elementAttributes)
import Text.XML.Cursor (
  Cursor,
  fromDocument, node,
  ($/), ($//), (&/), (&//),
  descendant, child, element, attribute)
import Data.ByteString.Lazy (ByteString, getContents)
import Data.Text (unpack)
import Text.Blaze.Html.Renderer.String (renderHtml)

import qualified MidiOutHtml
import Types
import Util


(|>) = flip ($)

getNodeAttr :: Name -> Node -> String
getNodeAttr name n =
  n |> nodeElem |> elementAttributes |> lookup name |> fromJust |> unpack

getValue :: Read a => a -> Node -> a
getValue fallback node = val
  where maybeVal = readMaybe $ getNodeAttr "Value" node
        val = case maybeVal of
          Nothing -> fallback
          Just x -> x

parseNote :: Node -> Note
parseNote n = Note time duration
  where time     = getTimeAttr "Time" n
        duration = getTimeAttr "Duration" n

toIntBeatTime :: Double -> Int
toIntBeatTime n = round (n * 128.0)

getTimeAttr :: Name -> Node -> Time
getTimeAttr name n = getNodeAttr name n |> read |> toIntBeatTime

nodeElem :: Node -> Element
nodeElem (NodeElement e) = e

fstNodeValue :: Read a => a -> [Cursor] -> a
fstNodeValue fallback (cursor:_) = cursor |> node |> (getValue fallback)

parseKeyTrack :: Cursor -> KeyTrack
parseKeyTrack cursor = KeyTrack key notes
  where key = (fstNodeValue 0) $ cursor $/ element "MidiKey"
        noteEvents = cursor $/ element "Notes" &/ element "MidiNoteEvent"
        notes = map (parseNote . node) noteEvents

parseClip :: Cursor -> Clip
parseClip cursor = (toIntBeatTime time, keyTracks)
  where time = (fstNodeValue (0::Double)) $ cursor $// element "CurrentStart"
        keyTracks = map parseKeyTrack $ cursor $/ element "Notes"
                                               &/ element "KeyTracks"
                                               &/ element "KeyTrack"

parseTrack :: Cursor -> Track
parseTrack cursor = Track name clips
  where name = nameCursor |> node |> getNodeAttr "Value"
        nameCursor:_ = cursor $/ element "Name" &/ element "EffectiveName"
        clips = map parseClip $ cursor $// element "MidiClip"

keyRange :: [KeyTrack] -> (MidiKey, MidiKey)
keyRange keyTracks = (minimum keys, maximum keys)
  where keys = map _key keyTracks

_endTime :: Note -> Time
_endTime note = _startTime note + _duration note

timeRange :: [KeyTrack] -> (Time, Time)
timeRange keyTracks = (0, maximum endTimes)
  where endTimes = concat $ map ((map _endTime) . _notes) keyTracks

mkKeyTrackSetView :: [KeyTrack] -> (Time, Time) -> [KeyTrackView]
mkKeyTrackSetView keyTracks timeRange
  = map mkView (reverse [startKey..endKey])
    where
      (startKey, endKey) = keyRange keyTracks
      mkView key = KeyTrackView key timeRange notes
        where
          notes = case filter ((== key) . _key) keyTracks of
            [] -> []
            kts -> mergeKeyTracks kts |> _notes |> inTimeWindow timeRange

inTimeWindow :: (Time, Time) -> [Note] -> [Note]
inTimeWindow (start, end) notes = filter f notes
  where f note = rangesOverlap (SpanRange start end)
                               (SpanRange (_startTime note) (_endTime note))

mergeKeyTracks :: [KeyTrack] -> KeyTrack
mergeKeyTracks keyTracks = foldr1 f keyTracks
  where f (KeyTrack key notes) (KeyTrack _ notes') = KeyTrack key (notes ++ notes')

trackKeyTracks :: Track -> [KeyTrack]
trackKeyTracks (Track name clips) = concat $ map f clips
  where f (offset, keyTracks) = map (g offset) keyTracks
        g offset (KeyTrack key notes) = KeyTrack key (map (h offset) notes)
        h offset (Note startTime duration) = Note (offset + startTime) duration

mkTrackView :: Int -> Track -> TrackView
mkTrackView blockLength track =
  TrackView (_trackName track) stuff
    where
      stuff = map block [0..nBlocks]
      block n = mkKeyTrackSetView keyTracks (n * blockLength, (n + 1) * blockLength)
      nBlocks = (endTime `div` blockLength) + 1
      (_, endTime) = timeRange keyTracks
      keyTracks = trackKeyTracks track


writeTracks :: [Track] -> Int -> String -> [IO ()]
writeTracks tracks blockLength filePrefix = map render (zip tracks [1..])
  where
    render (track, index) =
      writeFile fileName
                $ mkTrackView blockLength track
                  |> MidiOutHtml.render
                  |> renderHtml
        where fileName = filePrefix ++ "-" ++ (show index) ++ ".html"

isNotEmpty :: Track -> Bool
isNotEmpty track = case _clips track of
  [] -> False
  [(_, [])] -> False
  _ -> True

readArgs :: IO (String, Int)
readArgs = do
  args <- getArgs
  let filePrefix = case args of
                     (arg:_) -> arg
                     otherwise -> "out"
  let blockLength = case args of
                      (_:arg:_) -> (read arg)::Int
                      otherwise -> 128
  return (filePrefix, blockLength)


main = do
  input <- getContents :: IO ByteString
  (filePrefix, blockLength) <- readArgs
  let tracks = filter isNotEmpty
             $ map parseTrack
             $ fromDocument (parseLBS_ def input) $// element "MidiTrack"
  sequence_ $ writeTracks tracks blockLength filePrefix
