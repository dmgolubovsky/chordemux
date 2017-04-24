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
  ochan :: Event.Channel,
  dt :: Integer,
  age :: Integer,
  status :: DispStatus,
  ping :: Maybe Event.T
}

getps :: IO Integer

getps = getCurrentTime >>= return . diffTimeToPicoseconds . utctDayTime

data DispStatus = ChordOff | Pinged | Aging Int | ChordOn deriving (Eq, Ord)

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
    os = IS.empty,
    ochan = Event.Channel (-1),
    dt = dtt,
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
  cdtt <- lift getps
  pstt <- gets dt
  iss <- gets is
  let diff = round $ fromIntegral (cdtt - pstt) / 1000000
  let delay = diff * 10
  stt <- gets status
  case stt of
    Pinged -> do
      modify (\s -> s {status = Aging delay})
      lift $ threadDelay delay
      sendPing
      loop
    Aging d -> do
      if diff < d then do
        lift $ threadDelay delay
        sendPing
      else do
        if IS.size iss > 0 then do
          modify (\s -> s {status = ChordOn })
          crr <- gets cr
          let chints = toIntervals iss
              mboc = routeChord crr (Intervals chints)
          case mboc of
            Nothing -> return ()
            Just oc -> do
              let pc = buildChord iss oc
              modify (\s -> s {os = pc, ochan = outchan oc})
              cc <- gets c
              hh <- gets h
              lift $ playChord hh cc (outchan oc) pc True
              loop
        else do
          cancelChord
    _ -> return ()
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
  modify (\s -> s {is = is', dt = cdtt})
  stt <- gets status
  case stt of
    _ | stt /= Pinged -> do
      sendPing
      modify (\s -> s {status = Pinged })
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

sendPing = do
  hh <- gets h
  gets ping >>= return . maybeToList >>= mapM_ (lift . (Event.outputDirect hh))
  lift $ Event.drainOutput hh
  return ()


