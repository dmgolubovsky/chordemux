module ShowStatus where

import LoopConfig

import System.Console.ANSI
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import qualified Data.Map as DM
import qualified Data.IntSet as IS

import qualified Sound.ALSA.Sequencer.Event as Event

showstat :: StateT LoopStatus IO ()

showstat = do
  st <- get
  case statchg st of
    False -> return ()
    True -> do
      modify (\s -> s {statchg = False})
      lift $ do
        clearScreen
        setCursorPosition 0 0
        forM_ (DM.keys $ portmap st) $ \port -> do
          putStr (port ++ ": ")
          forM_ (os st) $ \(port', _, Event.Channel chan, chord) -> case port == port' of
            True -> putStr (show chan ++ " " ++ show (IS.toList chord))
            False -> return ()
          putStrLn ""
        putStrLn ("Samples: " ++ show (IS.size (tsamples st)))
        putStrLn ("Status: " ++ show (status st))
        putStrLn (statmsg st)
        return ()
