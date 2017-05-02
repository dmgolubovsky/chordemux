module LoopConfig where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Data.List
import Data.Maybe
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.IntSet as IS

import Control.Monad.Trans.Cont

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
  portmap :: DM.Map String Connect.T
}

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

