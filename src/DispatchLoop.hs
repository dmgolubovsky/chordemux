module DispatchLoop (dispatchLoop) where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Data.List as DL
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.IntSet as IS
import GHC.Word
import Data.Maybe
import Data.Time.Clock
import Control.Monad

import Numeric.Statistics

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import ChordData
import ChordUtils
import EventUtils
import LoopConfig

getps :: IO Integer

getps = getCurrentTime >>= return . diffTimeToPicoseconds . utctDayTime

dispatchLoop :: SndSeq.T SndSeq.DuplexMode -> 
                Connect.T -> 
                Event.Channel -> 
                ChordRouting -> IO ()

dispatchLoop h ci chan cr = do
  dtt <- getps
  evalStateT loop LoopStatus {
    h = h,
    ci = ci,
    chan = chan,
    cr = cr,
    is = IS.empty,
    pvl = 0,
    os = DS.empty,
    dt = dtt,
    diff = 0,
    tsamples = IS.empty,
    age = 0,
    status = ChordOff,
    ping = Nothing,
    portmap = DM.empty
}

loop :: StateT LoopStatus IO ()

loop = do
  ports <- gets cr >>= (return . listPorts)
  pm <- gets portmap
  let newports = ports DL.\\ DM.keys pm
  hh <- gets h
  newconns <- lift $ mapM (makePort hh) newports
  modify (\s -> s {portmap = DM.union pm (DM.fromList $ zip newports newconns)})
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
            _ -> do
              playChords mboc
              loop
    _ -> loop
  loop

playChords Nothing = loop

playChords (Just oc) = do
  pm <- gets portmap
  let mbcn = DM.lookup (outport oc) pm
  case mbcn of
    Nothing -> return ()
    Just cc -> do
      iss <- gets is
      let pc = buildChord iss oc
      oss <- gets os
      modify (\s -> s {os = DS.insert (cc, outchan oc, pc) oss})
      hh <- gets h
      lift $ playChord hh cc (outchan oc) pc True
  playChords $ also oc

processNote ne note = do
  cn <- gets chan
  hh <- gets h
  let pitch = Event.unPitch $ Event.noteNote note
      velo = Event.unVelocity $ Event.noteVelocity note
  let note' = Event.simpleNote cn 
                               (Event.Pitch pitch) 
                               (Event.noteVelocity note)
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
  hh <- gets h
  oss <- gets os
  forM_ oss $ \(cc, ochan, is) -> lift $ playChord hh cc ochan is False
  modify (\s -> s {is = IS.empty, os = DS.empty, status = ChordOff })
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
