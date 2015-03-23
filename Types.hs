module Types where

type MidiKey  = Int
type Time     = Int

data Note     = Note
  { _startTime :: Time
  , _duration :: Time
  } deriving Show

data KeyTrack = KeyTrack
  { _key :: MidiKey
  , _notes :: [Note]
  } deriving Show

type Clip = (Time, [KeyTrack])

data Track = Track
  { _trackName :: String
  , _clips :: [Clip]
  } deriving Show

data TrackView = TrackView String [[KeyTrackView]]
  deriving Show

data KeyTrackView = KeyTrackView
  { _viewKey      :: MidiKey
  , _viewInterval :: (Time, Time)
  , _viewNotes    :: [Note]
  } deriving Show
