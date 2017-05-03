import ChordData
import ChordUtils
import EventUtils
import Common (handleExceptionCont)
import DispatchLoop
import ChordMatrix

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
import Data.Map as DM

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
  putStrLn ("Created sequencer with id: " ++ show c)
  let connin = Connect.toSubscribers (Addr.Cons c pin)
  let dl = dispatchLoop h connin (Event.Channel 0)
  dl "xxx"



