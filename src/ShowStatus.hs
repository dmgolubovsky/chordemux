module ShowStatus where

import LoopConfig

import System.Console.ANSI
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.IntSet as IS
import qualified Data.Sequence as SQ
import qualified Data.Foldable as DF

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
        putStrLn ("mean: " ++ show (meann $ stats $ st) ++ 
                  " sd: " ++ show (stddevv $ stats $ st) ++
                  " min: " ++ show (minn $ stats $ st) ++
                  " max: " ++ show (maxx $ stats $ st))
        putStrLn ("Status: " ++ show (status st))
        putStrLn ("History: " ++ (DL.intercalate " " $ map showint (DF.toList (chist st))))
        putStrLn (statmsg st)
        return ()

showint is = DL.intercalate "-" $ map show is
