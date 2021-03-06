module ChordData where

import Data.Map
import Sound.ALSA.Sequencer.Event (Pitch, Channel)

type Interval = Int

-- Input chord which is represented by its list of intervals or symbolic shorthand.
-- Received chords are matched against the list of input chords to find the dispatch
-- rules. Additionally, an entry matching any chord may be provided.

data InputChord = AnyChord 
  | Intervals [Interval] [[Interval]] 
  | Shorthand String deriving (Show, Eq, Ord)

intervals = flip Intervals ([])

-- Output chord which is represented by the destination channel, and notes to be sent.
-- Not all notes of the matched chord may need to be sent, it may be an unrelated absolute
-- note, or one or more notes of the actual chord with possible offset. A shortcut 
-- constructor AllNotes exists to send all notes of the chord.

data NoteOut = AbsNote Pitch |
               AllNotes | 
               ChordNote {
                 idx :: Int,      -- 1-based
                 offset :: Int
               } deriving (Show)

-- How to play the chord. Chords may go to multiple ports at once, so the 
-- 'also' field serves as a chain link to the next port to output the chord.

data OutputChord = OutputChord {
  outport :: String,
  outchan :: Channel,
  outnotes :: [NoteOut],
  also :: Maybe OutputChord
} deriving (Show)

-- Routing rules represented as a map of input to output chords.

type ChordRouting = Map InputChord OutputChord


