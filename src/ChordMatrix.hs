module ChordMatrix where

import ChordData
import ChordUtils

import Data.Map as DM hiding (map)
import Sound.ALSA.Sequencer.Event (Channel(..))

chordMatrix = DM.fromList $ zip keys values where
  values = map (\c -> OutputChord {outport = "Rhytm", 
                                   also = Just $ OutputChord {
                                     outport = "Bass",
                                     outchan = Channel 0,
                                     also = Nothing,
                                     outnotes = [ChordNote {idx = 1, offset = 0},
                                                 ChordNote {idx = 3, offset = 0}]
                                   }, 
                                   outnotes = [AllNotes], 
                                   outchan = c}) $ map Channel [0 .. ]
  keys = majors ++ dim ++ minors ++ any ++ maj7 ++ min7
  majors = map Intervals $ findInversionsI [4, 3]
  dim = [Intervals [3, 3]]
  minors = map Intervals $ findInversionsI [3, 4]
  any = [AnyChord]
  maj7 = map Intervals $ findInversionsI [4, 3, 3]
  min7 = map Intervals $ findInversionsI [3, 4, 3]

