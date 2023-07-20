module Main where

import Data.Time (formatTime,getZonedTime,defaultTimeLocale)
import qualified Data.Map as M

import Control.Monad (forM_,when)
import Control.Monad.State
import Control.Concurrent (threadDelay,ThreadId,myThreadId)

import qualified System.Console.Terminal.Size as T
import qualified System.Console.ANSI as ANS

import System.Exit 
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import qualified Control.Exception as E

import Config
import Render ( renderClock )

data ConsoleState = ConsoleState {
                                       wSize   :: (Integer,Integer)
                                     , config  :: YMLConfig
                                     , charLUT :: M.Map Char [Int]
                                     , timeStr :: String
                                }

getTime::String->IO String
getTime timeStr = formatTime defaultTimeLocale timeStr <$> getZonedTime

getSize::IO (Integer,Integer)
getSize = do
           size <- T.size
           case size of
             Just T.Window{T.height=h, T.width=w} -> return (w,h)
             Nothing                              -> error "Couldn't get window size"

drawTimeString :: ConsoleState -> IO ()
drawTimeString state = do
                  let ConsoleState {wSize=_wSize, config=_config, charLUT=_charLUT, timeStr=_timeStr} = state
                  let (mat,offs) = renderClock _config _timeStr  _wSize _charLUT
                  ANS.setCursorPosition  (snd offs) 0

                  forM_ mat $ \x->do 
                      ANS.setCursorColumn (fst offs)
                      putStrLn   x

updateClock :: StateT ConsoleState IO ()
updateClock = do
                cs@ConsoleState {wSize=_wSize, config=_config,  timeStr=_timeStr} <- get
                currentSize <- liftIO  getSize
                time        <- liftIO $ getTime (timeDisplay _config)
                
                let timeChanged = time /= _timeStr
                let winChanged  = currentSize /= _wSize
                 
                let currentState = cs{wSize=currentSize,timeStr=time} 

                when winChanged $  liftIO ANS.clearScreen 
                when (timeChanged || winChanged) $ liftIO (drawTimeString currentState)
                
                put currentState
                liftIO $ threadDelay (updateDelay _config)
                updateClock

setColor :: ANS.ConsoleLayer -> ConsoleColor -> IO ()
setColor part color  = ANS.setSGR [ANS.SetColor part (intensity color) (colorName color)]

handleShutdown :: ThreadId -> IO ()
handleShutdown mainThreadID= do
                               ANS.showCursor
                               ANS.setCursorPosition 0 0
                               ANS.clearScreen
                               E.throwTo mainThreadID ExitSuccess

main :: IO ()
main = do
        cfg <- loadYML "config.yml"
        
        setColor ANS.Foreground (foreground cfg)
        setColor ANS.Background (background cfg)
        ANS.hideCursor
        
        tid <- myThreadId
        installHandler sigINT (Catch $  handleShutdown tid) Nothing
        installHandler sigTERM (Catch $ handleShutdown tid) Nothing

        evalStateT updateClock $ ConsoleState (0,0) cfg (getLUT cfg) "" 