{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DispatchLoop (dispatchLoop) where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.IntSet as IS
import qualified Data.Sequence as SQ
import GHC.Word
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Monad

import Statistics

import Control.Exception
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import ChordData
import ChordUtils
import EventUtils
import LoopConfig
import ChordMatrix
import ShowStatus

import WithCli

data Options = Options {
  channel :: Maybe Int,
  config :: Maybe String,
  cmdelay :: Maybe Int,
  codelay :: Maybe Int,
  histlen :: Maybe Int
} deriving (Show, Generic, HasArguments)

getus :: IO Integer

getus = (*1000000) `fmap` getPOSIXTime >>= return . round

loadcr :: Maybe String -> IO (ChordRouting, Maybe UTCTime)

loadcr Nothing = return (chordMatrix, Nothing)
loadcr (Just f) = tryLoad f `catch` \(e :: SomeException) -> do
  putStrLn $ show e
  cfgt <- getCfgTime f
  return (chordMatrix, cfgt)

fromMaybeMax n d mbn = max n (fromMaybe d mbn)

dispatchLoop :: SndSeq.T SndSeq.DuplexMode -> 
                Connect.T -> 
                Options -> IO ()

dispatchLoop h ci opt@Options {channel = chan, config = s} = do
  dtt <- getus
  (cr, mbcfgt) <- loadcr s
  evalStateT loop LoopStatus {
    h = h,
    ci = ci,
    chan = Event.Channel $ fromIntegral $ fromMaybe 0 chan,
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
    ckcfg = Nothing,
    cfgtime = mbcfgt,
    cfgpath = s,
    cmdly = fromMaybeMax 1 10 (cmdelay opt),
    codly = fromMaybeMax 1 10 (codelay opt),
    portmap = DM.empty,
    statmsg = "",
    statchg = False,
    chstlen = fromMaybeMax 1 3 (histlen opt),
    chist = SQ.empty,
    stats = Stats 0 0 0 0
}

loop :: StateT LoopStatus IO ()

loop = do
  showstat
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
    Nothing | ckcfgEvent (Event.body e) -> checkConfig
    Just ec | ec == cn -> do
      if isNothing pn then (do
        let pn' = e {Event.body = Event.CustomEv Event.User0 Event.customZero}
        let ck' = e {Event.body = Event.CustomEv Event.User1 Event.customZero}
        modify (\s -> s {ping = Just pn', ckcfg = Just ck'})
        sendPingDelay' ckcfg 1000000)
      else return ()
      loop' e
    _ -> loop
  
loop' e = do
  case Event.body e of
    Event.CtrlEv ce ctrl | allNotesOff ce ctrl -> cancelChord
    Event.NoteEv ne note -> processNote ne note
    _ -> do
      loop

checkConfig = do
  cfgp <- gets cfgpath
  cfgt <- gets cfgtime
  case (cfgp, cfgt) of
    (Just fp, Just ft) -> do
      mbnwt <- lift $ getCfgTime fp
      case mbnwt of
        Just nwt -> do
          if nwt > ft 
            then do
              crr <- gets cr
              (ncr, mbxx) <- lift (tryLoad fp `catch` 
                                    (\(_::SomeException) -> return (crr, Nothing)))
              let sucf = case mbxx of
                           Just _ -> "success"
                           Nothing -> "failure"
              updstat ("reload config " ++ sucf)
              modify (\s -> s {cr = ncr, cfgtime = Just nwt})
            else return ()
          sendPingDelay' ckcfg 1000000
          loop
        _ -> loop
    _ -> loop

handleChords = do
  iss <- gets is
  ddff <- gets diff
  let dxff = fromIntegral ddff
  stt <- gets status
  hh <- gets h
  pvll <- gets pvl
  modify (\s -> s {pvl = IS.size iss})
  case stt of
    ChordOn d -> do
      case (IS.size iss `compare` pvll) of
        GT -> do
          updstat "chord growing while playing ignore"
          sendPingDelay d
          loop
        LT -> do
          updstat "chord dropping"
          modify (\s -> s {status = ChordOff, is = IS.empty})
          cancelChord
          loop
        EQ -> do
          modify (\s -> s {status = ChordOn d})
          sendPingDelay d
          loop
    Pinged -> case IS.size iss of
      _ | IS.size iss < pvll -> do
        modify (\s -> s {status = ChordOff, is = IS.empty})
        loop
      0 -> do
        modify (\s -> s {status = ChordOff})
        cancelChord
        loop
      1 -> do
        modify (\s -> s {status = ChordOff})
        loop
      _ | IS.size iss >= 2 -> do
        updstat "chord mature"
        ndxff <- compStats dxff
        let delay = ndxff * 10
        modify (\s -> s {status = Aging delay})
        sendPingDelay delay
        loop
    Aging d -> do
      case (IS.size iss `compare` pvll) of
        GT -> do
          sendPingDelay d
          loop
        LT -> do
          modify (\s -> s {status = ChordOff, is = IS.empty})
          cancelChord
          loop
        EQ -> do
          modify (\s -> s {status = ChordOn d})
          sendPingDelay d
          crr <- gets cr
          hist <- gets chist >>= return . DF.toList
          let chints = toIntervals iss
              mboc = routeChord hist crr (intervals chints)
          case mboc of
            Nothing -> do
              modify (\s -> s {status = ChordOff, is = IS.empty })
              loop
            _ -> do
              addHistory chints
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
      modify (\s -> s {statchg = True, os = DS.insert (outport oc, cc, outchan oc, pc) oss})
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
  cdtt <- lift getus
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
  forM_ oss $ \(_, cc, ochan, is) -> lift $ playChord hh cc ochan is False
  modify (\s -> s {statchg = True, is = IS.empty, os = DS.empty, status = ChordOff })
  loop

sendPingDelay d = sendPingDelay' ping d

sendPingDelay' ppp d = do
  hh <- gets h
  p <- gets ppp
  lift $ forkOS $ do
    threadDelay d
    sendPing hh p
  return ()

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
  let newmean = mean keep
  let newsamp = IS.fromList $ map round keep
  let newsamp' = if IS.size newsamp > 50 then IS.delete (IS.findMax newsamp) newsamp else newsamp
  modify (\s -> s {tsamples = newsamp', stats = Stats {
                                                  meann = tmean,
                                                  stddevv = tstd,
                                                  minn = fromIntegral $ IS.findMin samples',
                                                  maxx = fromIntegral $ IS.findMax samples'
                                                      }})
  return $ round newmean
