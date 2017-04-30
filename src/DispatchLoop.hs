module DispatchLoop (dispatchLoop) where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Data.IntSet as IS
import GHC.Word
import Data.Maybe
import Data.Time.Clock

import Numeric.Statistics

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import ChordData
import ChordUtils
import EventUtils

data LoopStatus = LoopStatus {
  h :: SndSeq.T SndSeq.DuplexMode,
  c :: Connect.T,
  ci :: Connect.T,
  chan :: Event.Channel,
  cr :: ChordRouting,
  is :: IS.IntSet,
  os :: IS.IntSet,
  pvl :: Int,
  ochan :: Event.Channel,
  dt :: Integer,
  diff :: Integer,
  tsamples :: IS.IntSet,
  age :: Integer,
  status :: DispStatus,
  ping :: Maybe Event.T
}

getps :: IO Integer

getps = getCurrentTime >>= return . diffTimeToPicoseconds . utctDayTime

data DispStatus = ChordOff | Pinged | Aging Int | ChordOn Int deriving (Show, Eq, Ord)

dispatchLoop :: SndSeq.T SndSeq.DuplexMode -> 
                (Connect.T, Connect.T) -> 
                Event.Channel -> 
                ChordRouting -> IO ()

dispatchLoop h (c, ci) chan cr = do
  dtt <- getps
  evalStateT loop LoopStatus {
    h = h,
    c = c,
    ci = ci,
    chan = chan,
    cr = cr,
    is = IS.empty,
    pvl = 0,
    os = IS.empty,
    ochan = Event.Channel (-1),
    dt = dtt,
    diff = 0,
    tsamples = IS.empty,
    age = 0,
    status = ChordOff,
    ping = Nothing
}

loop :: StateT LoopStatus IO ()

loop = do
  cn <- gets chan
  pn <- gets ping
  e <- gets h >>= lift . Event.input
  case eventChannel e of
    Nothing | pingEvent (Event.body e) -> handleChords
    Just ec | ec == cn -> do
      if isNothing pn then (do
        let pn' = e {Event.body = Event.CustomEv Event.User0 Event.customZero}
        modify (\s -> s {ping = Just pn'}))
      else do
        return ()
      loop' e
    _ -> loop
  
loop' e = do
  case Event.body e of
    Event.CtrlEv ce ctrl | allNotesOff ce ctrl -> cancelChord
    Event.NoteEv ne note -> processNote ne note
    _ -> do
      loop

handleChords = do
  iss <- gets is
  ddff <- gets diff
  let dxff = round $ fromIntegral ddff / 1000000
  stt <- gets status
  hh <- gets h
  pvll <- gets pvl
  modify (\s -> s {pvl = IS.size iss})
  case stt of
    ChordOn d -> do
      case (IS.size iss `compare` pvll) of
        GT -> do
          lift $ putStrLn "chord growing while playing ignore"
          sendPingDelay d
          loop
        LT -> do
          lift $ putStrLn "chord dropping"
          modify (\s -> s {status = ChordOff, is = IS.empty})
          cancelChord
          loop
        EQ -> do
          lift $ putStrLn ("chord on " ++ (show $ IS.toList iss))
          modify (\s -> s {status = ChordOn d})
          sendPingDelay d
          loop
    Pinged -> case IS.size iss of
      _ | IS.size iss < pvll -> do
        modify (\s -> s {status = ChordOff, is = IS.empty})
        loop
      0 -> do
        lift $ putStrLn "chord empty"
        modify (\s -> s {status = ChordOff})
        cancelChord
        loop
      1 -> do
        lift $ putStrLn ("single note need more " ++ show dxff)
        modify (\s -> s {status = ChordOff})
        loop
      _ | IS.size iss >= 2 -> do
        lift $ putStrLn ("two or more notes calibrating delay " ++ show dxff)
        ndxff <- compStats dxff
        let delay = ndxff * 10
        modify (\s -> s {status = Aging delay})
        sendPingDelay delay
        loop
    Aging d -> do
      case (IS.size iss `compare` pvll) of
        GT -> do
          lift $ putStrLn "chord growing"
          sendPingDelay d
          loop
        LT -> do
          lift $ putStrLn "chord dropping"
          modify (\s -> s {status = ChordOff, is = IS.empty})
          cancelChord
          loop
        EQ -> do
          lift $ putStrLn ("chord mature " ++ (show $ IS.toList iss))
          modify (\s -> s {status = ChordOn d})
          sendPingDelay d
          crr <- gets cr
          let chints = toIntervals iss
              mboc = routeChord crr (Intervals chints)
          case mboc of
            Nothing -> do
              modify (\s -> s {status = ChordOff, is = IS.empty })
              loop
            Just oc -> do
              let pc = buildChord iss oc
              modify (\s -> s {os = pc, ochan = outchan oc})
              cc <- gets c
              hh <- gets h
              lift $ playChord hh cc (outchan oc) pc True
              loop
    _ -> loop
  loop

processNote ne note = do
  cn <- gets chan
  cc <- gets c
  hh <- gets h
  let pitch = Event.unPitch $ Event.noteNote note
      velo = Event.unVelocity $ Event.noteVelocity note
  let note' = Event.simpleNote cn 
                               (Event.Pitch pitch) 
                               (Event.noteVelocity note)
  let eo = Event.forConnection cc $ Event.NoteEv ne note'
  iss <- gets is
  let is' = compactChord $ case (ne, velo) of
              (Event.NoteOn, 0) -> IS.delete (fromIntegral pitch) iss
              (Event.NoteOn, _) -> IS.insert (fromIntegral pitch) iss
              (Event.NoteOff, _) -> IS.delete (fromIntegral pitch) iss
              _ -> iss
  cdtt <- lift getps
  pdtt <- gets dt
  modify (\s -> s {is = is', dt = cdtt})
  case IS.size is' of
    0 -> return ()
    1 -> modify (\s -> s {diff = 0})
    _ -> modify (\s -> s {diff = cdtt - pdtt})
  stt <- gets status
  case stt of
    _ | stt `elem` [ChordOff] -> do
      sendPingDelay 0
      modify (\s -> s {status = Pinged })
      return ()
    _ -> return ()
  loop


cancelChord = do
  cc <- gets c
  hh <- gets h
  ochan <- gets ochan
  oss <- gets os
  lift $ playChord hh cc ochan oss False
  modify (\s -> s {is = IS.empty, os = IS.empty, status = ChordOff })
  loop

sendPingDelay d = do
  hh <- gets h
  p <- gets ping
  lift $ forkOS $ do
    threadDelay d
    sendPing hh p

sendPing hh ping = do 
  case ping of
    Just p -> do
      Event.outputDirect hh p
      return ()
    _ -> return ()
  Event.drainOutput hh
  return ()

compStats d = do
  samples <- gets tsamples
  let samples' = IS.insert d samples
  let st = map fromIntegral $ IS.toList samples'
  let tmean = mean st
  let tstd = stddev st
  let outlrs = filter (> (tmean + 3 * tstd)) st
  let keep = if length st > 10 
               then filter (<= (tmean + 3 * tstd)) st 
               else st
  lift $ putStrLn ("size " ++ show (IS.size samples') ++ 
                   " mean " ++ show tmean ++ 
                   " std " ++ show tstd ++
                   " outliers " ++ show outlrs);
  let newmean = mean keep
  let newsamp = IS.fromList $ map round keep
  let newsamp' = if IS.size newsamp > 50 then IS.delete (IS.findMax newsamp) newsamp else newsamp
  modify (\s -> s {tsamples = newsamp'})
  lift $ putStrLn ("new mean " ++ show newmean)
  return $ round newmean
