{-# LANGUAGE QuasiQuotes #-} -- for hamlet

module MidiOutHtml where

import Prelude (show, (-))
import GHC.Base
import Text.Hamlet (shamlet)
import Text.Blaze.Html (Html)
import Data.Tuple (fst, snd)

import Types
import Util


renderKeyTrackView :: KeyTrackView -> Html
renderKeyTrackView (KeyTrackView key (startTime, endTime) notes) = [shamlet|
<div .key-track data-key=#{key} data-start-time=#{startTime} data-end-time=#{endTime}>
    $forall note <- notes
        #{renderNote key (startTime, endTime) note}
|]

renderNote :: MidiKey -> (Time, Time) -> Note -> Html
renderNote key (intervalStart, intervalEnd) (Note startTime duration) = [shamlet|
<div .note data-key=#{key} data-start-time=#{startTime} data-duration=#{duration} style="position: absolute; left: #{relStart}%; width: #{relDuration}%;">
|]
  where relStart    = asPercentageOf blockLength (startTime - intervalStart)
        relDuration = asPercentageOf blockLength duration
        blockLength = intervalEnd - intervalStart

render :: TrackView -> Html
render (TrackView name stuff) = [shamlet|
<html>
    <head>
      <link href="MidiOut.css" rel="stylesheet">
    <body>
      <h1>#{name}
      $forall ktsv <- stuff
        <div .key-track-set>
          $forall ktv <- ktsv
            ^{renderKeyTrackView ktv}
|]
