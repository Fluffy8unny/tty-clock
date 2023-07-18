module Main where

import Data.Time (formatTime,getZonedTime,defaultTimeLocale)
import Control.Monad (forever,forM_,when)
import Control.Monad.State
import Control.Concurrent (threadDelay)
import qualified System.Console.Terminal.Size as T
import qualified System.Console.ANSI as ANS
import qualified Data.Map as M

import Config
import Render ( renderClock )


getTime::String->IO String
getTime timeStr = formatTime defaultTimeLocale timeStr <$> getZonedTime

getSize::IO (Integer,Integer)
getSize = do
           size <- T.size
           case size of
             Just T.Window{T.height=h, T.width=w} -> return (w,h)
             Nothing                              -> error "Couldn't get window size"

waitForNextChange::IO ()
waitForNextChange = do
                picoS <- getTime "%q" 
                let waitTime =  (10^12 -  (read::String->Int) picoS) `div` 10^6
                threadDelay waitTime

data ConsoleState = ConsoleState {
                                       wSize   :: (Integer,Integer)
                                     , config  :: YMLConfig
                                     , charLUT :: M.Map Char [Int]
                                }

updateClock :: StateT ConsoleState IO ()
updateClock = do
                cs@ConsoleState {wSize=_wSize, config=_config, charLUT=_charLUT} <- get
                currentSize <- liftIO  getSize
                when (currentSize /= _wSize) $ do 
                    liftIO ANS.clearScreen 
                put cs{wSize=currentSize}

                time       <- liftIO $ getTime (timeDisplay _config)
                let (mat,offs) = renderClock _config time  currentSize _charLUT
                liftIO $ ANS.setCursorPosition  (snd offs) 0

                forM_ mat $ \x->do 
                    liftIO $ ANS.setCursorColumn (fst offs)
                    liftIO $ putStrLn   x

                liftIO waitForNextChange
                updateClock
main :: IO ()
main = do
        cfg <- loadYML "config.yml"
        let state =  ConsoleState (0,0) cfg (getLUT cfg)
        evalStateT updateClock state 
