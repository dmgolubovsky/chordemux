import ChordData
import ChordUtils
import EventUtils
import Common (handleExceptionCont)
import DispatchLoop
import ChordMatrix

import Paths_chordemux

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )

import Control.Concurrent

import System.Environment (getArgs, )

import Data.Bits
import Data.List
import Data.Maybe
import Data.Version
import Data.Map as DM

import WithCli

import qualified Data.IntSet as IS

main :: IO ()
main = handleExceptionCont $ do
  liftIO $ putStrLn "Starting."
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "ChordDeMux"
  pin <-
     ContT $
     Port.withSimple h "in"
        (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric
  liftIO $ mainIO h pin 

mainIO :: SndSeq.T SndSeq.DuplexMode -> Port.T -> IO ()
mainIO h pin = do
  c <- Client.getId h
  let connin = Connect.toSubscribers (Addr.Cons c pin)
  let dl = dispatchLoop h connin
  withCliModified mods dl

mods = [
  AddVersionFlag $ showVersion version,
  AddShortOption "channel" 'C',
  AddOptionHelp "channel" "MIDI channel to receive messages on",
  AddOptionHelp "config" "configuration file in YAML format",
  AddOptionHelp "cmdelay" "delay multiplier while chord is maturing",
  AddOptionHelp "codelay" "delay multiplier while chord is on",
  AddOptionHelp "histlen" "chord history length",
  AddShortOption "config" 'f']

