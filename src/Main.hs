import ChordData
import ChordUtils
import EventUtils
import Common (handleExceptionCont)
import DispatchLoop

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
  pout <-
     ContT $
     Port.withSimple h "out"
        (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  pin <-
     ContT $
     Port.withSimple h "in"
        (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric
  liftIO $ mainIO h pin pout

mainIO :: SndSeq.T SndSeq.DuplexMode -> Port.T -> Port.T -> IO ()
mainIO h pin pout = do
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  let connout = Connect.toSubscribers (Addr.Cons c pout)
      connin = Connect.toSubscribers (Addr.Cons c pin)

  let cr = DM.fromList [
             (Intervals [4, 3, 5, 4, 3], OutputChord (Event.Channel 5) [AllNotes]),
             (Intervals [3, 5], OutputChord (Event.Channel 8) 
                      [ChordNote {idx=1, offset=0}, ChordNote {idx=3, offset=1}]),
             (Intervals [5, 3], OutputChord (Event.Channel 7) [AllNotes]),
             (Intervals [4, 5], OutputChord (Event.Channel 7) [AllNotes])
           ]

  dispatchLoop h (connout, connin) (Event.Channel 0) cr


