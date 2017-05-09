{-# LANGUAGE ScopedTypeVariables #-}
module LoopConfig where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Data.Text (unpack)
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.IntSet as IS
import Data.Time.Clock

import System.Directory

import Data.Yaml.Config as Y

import Control.Exception

import Text.ParserCombinators.HuttonMeijer

import qualified Data.ByteString.Lazy as B

import Common (handleExceptionCont)
import ChordData

data DispStatus = ChordOff | Pinged | Aging Int | ChordOn Int deriving (Show, Eq, Ord)

data LoopStatus = LoopStatus {
  h :: SndSeq.T SndSeq.DuplexMode,
  ci :: Connect.T,
  chan :: Event.Channel,
  cr :: ChordRouting,
  is :: IS.IntSet,
  os :: DS.Set (Connect.T, Event.Channel, IS.IntSet),
  pvl :: Int,
  dt :: Integer,
  diff :: Integer,
  tsamples :: IS.IntSet,
  age :: Integer,
  status :: DispStatus,
  ping :: Maybe Event.T,
  ckcfg :: Maybe Event.T,
  cfgpath :: Maybe String,
  cfgtime :: Maybe UTCTime,
  cmdly :: Int,
  codly :: Int,
  portmap :: DM.Map String Connect.T
}

-- Given config file path, return its modification time or nothing if any error

getCfgTime :: String -> IO (Maybe UTCTime)

getCfgTime fp = 
  (getModificationTime fp >>= return . Just) `catch` (\(_::SomeException) -> return Nothing)

-- List all ports defined by the chord routing rules.

listPorts :: ChordRouting -> [String]

listPorts cr = nub $ lp $ DM.elems cr where
  lp [] = []
  lp (oc:ocs) = ports (Just oc) ++ lp ocs
  ports Nothing = []
  ports (Just oc) = outport oc : (ports $ also oc)

-- Create an output port with given name.

makePort :: SndSeq.T SndSeq.DuplexMode -> String -> IO Connect.T

makePort h s = do
  putStrLn ("create port " ++ s)
  pout <- Port.createSimple h s
    (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  c <- Client.getId h
  return $ Connect.toSubscribers (Addr.Cons c pout)

-- Try to load a YAML config file but in case of error throw an exception.

tryLoad :: String -> IO (ChordRouting, Maybe UTCTime)

tryLoad c = do
  cfg <- load c
  let kks = keys cfg
      ks = map unpack kks
      pchds = map (papply parseChord) ks
  ichords <- forM (zip pchds ks) $ \(pchd, k) -> case pchd of
    [(res, rem)] | rem == "" -> return res
    _ -> fail ("Could not parse chord " ++ k)
  ochords <- forM kks $ \k -> do
    ccfg <- subconfig k cfg
    let ckks = keys ccfg
        cks = map unpack ckks
        pprts = map (papply parsePort) cks
    ports <- forM (zip pprts cks) $ \(pprt, ck) -> case pprt of
      [(res, rem)] | rem == "" -> return res 
      _ -> fail ("Could not parse port " ++ ck)
    ontch' <- forM ckks $ \kk -> Y.lookup kk ccfg >>= 
               return . (papply parseNoteOut) >>= 
               mapM (return . fst)
    let ontch = map listToMaybe ontch'
    forM_ (zip3 ks ports ontch) $ \(a, b, c) -> case c of
      Just _ -> return ()
      Nothing -> fail ("could not parse output notes for chord " ++ show a ++ " port " ++ show b)
    let prtout = zip ports (map fromJust ontch)
    mkChordOut (unpack k) prtout
  cfgt <- getCfgTime c
  return $ (DM.fromList $ zip ichords ochords, cfgt)

mkChordOut :: String -> [(String, ([NoteOut], Event.Channel))] -> IO OutputChord

mkChordOut ch [] = fail ("no output notes for chord " ++ show ch)
mkChordOut ch [(port, (notes, chan))] = return $ OutputChord {
  outport = port,
  outchan = chan,
  outnotes = notes,
  also = Nothing}

mkChordOut ch (x:xs) = do
  och1 <- mkChordOut ch [x]
  och2 <- mkChordOut ch xs
  return och1 {also = Just och2}
  

-- Primitive parsers for pieces of the config.

-- Port: any name starting with an uppercase letter

parsePort :: Parser String

parsePort = do {x <- upper; xs <- many alphanum; return (x:xs)}

-- Chord: numbers separated by hyphens.

parseChord :: Parser InputChord

parseChord = any +++ ints where
  any = string "Any" >> return AnyChord
  ints = sepby1 nat (char '-') >>= return . Intervals
        
-- Output note: absolute note as dollar sign with number, 
--              chord note as number +/- number,
--              all notes as "All"
-- separated by spaces
-- followed by slash and channel number

parseNoteOut :: Parser ([NoteOut], Event.Channel)

parseNoteOut = do
  let absn = char '$' >> nat >>= return . AbsNote . Event.Pitch . fromIntegral
      all = string "All" >> return AllNotes
      chrdn = do
        note <- nat
        sign <- char '-' +++ char '+'
        let mlt = case sign of
                    '-' -> (-1)
                    '+' -> 1
        off <- nat
        return $ ChordNote {idx = note, offset = mlt * off}
  nns <- (absn +++ chrdn +++ all) `sepby1` spaces
  many (char ' ')
  chan <- char '/' >> nat >>= return . Event.Channel . fromIntegral
  return (nns, chan)

