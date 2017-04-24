module ChordUtils where

import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Event as Event
import Data.IntSet as IS
import Data.Map as DM
import Data.IntMap as IM
import Control.Monad (mplus)
import GHC.Word
import Data.List
import ChordData


-- Send Note On or Note Off for all notes in the given chord (IntSet).
-- Velocity is set as 64, it really does not matter here.

playChord :: SndSeq.AllowOutput mode => SndSeq.T mode -> 
                                        Connect.T -> 
                                        Event.Channel -> 
                                        IntSet -> 
                                        Bool -> 
                                        IO ()

playChord _ _    (Event.Channel (-1)) _ _ = return ()
playChord h conn chan is on = do
  let note pitch = Event.simpleNote chan 
                                    (Event.Pitch $ fromIntegral pitch) 
                                    (Event.Velocity 64)
      noo = if on then Event.NoteOn else Event.NoteOff
      evts = Prelude.map (Event.forConnection conn . Event.NoteEv noo . note) $ IS.toList is
  mapM_ (Event.outputDirect h) evts

-- Transpose all notes in the chord to be no more than two octaves higher than the lowest note

compactChord :: IntSet -> IntSet

compactChord is = 
  let lowest = IS.findMin is
      down n = lowest + ((n - lowest) `mod` 24)
  in  IS.map down is

-- Find all inversions of the chord. Compact if needed. Result of the compaction is
-- the root position. Add 12 to the lowest note to get the next inversion. Return
-- as many inversions as many notes the chord has.

findInversions :: IntSet -> [IntSet]

findInversions dch = 
  let dch' = compactChord dch
      invs = take (IS.size dch' - 1) [1..]
      nextinv is = IS.insert newmin is' where
        oldmin = IS.findMin is
        is' = IS.delete oldmin is
        newmin = oldmin + 12
      doinv _ [] = []
      doinv ch (x:xs) = let ch' = nextinv ch in ch' : doinv ch' xs
  in dch' : doinv dch' invs

-- Find an inversions of the chord given in intervals.

findInversionsI :: [Interval] -> [[Interval]]

findInversionsI is = Prelude.map toIntervals $ fis is where
  fis = findInversions . fromIntervals 0

-- Find all intervals between consecutive notes in a chord.

toIntervals :: IntSet -> [Interval]

toIntervals is = 
  let minnote = IS.findMin is
      is' = IS.map (subtract minnote) is
      sorted = sort $ IS.toList is
      toi [] = []
      toi [x] = []
      toi (x:y:ys) = (y - x) : (toi (y:ys))
    in toi sorted

-- Build a chord from intervals given the root note.

fromIntervals :: Int -> [Interval] -> IntSet

fromIntervals r is = IS.fromList $ r : (fi r is) where
  fi r [] = []
  fi r (x:xs) = (r + x) : fi (r + x) xs

-- Find how to output a chord given the set of routing rules. If AnyChord entry exists
-- in the map, it is given a last chance.

routeChord :: ChordRouting -> InputChord -> Maybe OutputChord

routeChord cr ic = some `mplus` any where
  some = DM.lookup ic cr
  any = DM.lookup AnyChord cr

-- Build an output chord set of notes based on the input chord notes and the output
-- chord rules.

buildChord :: IntSet -> OutputChord -> IntSet

buildChord is oc = bc on where
  ins = IM.fromList ([1 .. ] `zip` (IS.toAscList is))
  on = outnotes oc
  bc [] = IS.empty
  bc (n:ns) = (bc' n) `IS.union` (bc ns)
  bc' AllNotes = is
  bc' (AbsNote p) = IS.singleton $ fromIntegral $ Event.unPitch p
  bc' (cn@ChordNote {}) = case IM.lookup (idx cn) ins of
    Nothing -> IS.empty
    Just p -> IS.singleton (p + offset cn)


