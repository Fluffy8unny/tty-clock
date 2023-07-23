module Main where

import Data.Time (formatTime,getZonedTime,defaultTimeLocale)
import qualified Data.Map as M

import Control.Monad (forM_,when)
import Control.Monad.State
import Control.Concurrent (threadDelay,ThreadId,myThreadId)

import Paths_TTYClock

import qualified System.Console.Terminal.Size as T
import qualified System.Console.ANSI as ANS

import System.Exit 
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import qualified Control.Exception as E

import Config
import Render ( renderClock, getDateOffset )


getSize::IO (Integer,Integer)
getSize = do
           size <- T.size
           case size of
             Just T.Window{T.height=h, T.width=w} -> return (w,h)
             Nothing                              -> error "Couldn't get window size"

configureTerminal :: YMLConfig -> IO ()
configureTerminal cfg = let
                          setColor part color  = ANS.setSGR [ANS.SetColor part (intensity color) (colorName color)]
                         in do
                            setColor ANS.Foreground (foreground cfg)
                            setColor ANS.Background (background cfg)
                            ANS.hideCursor


data ConsoleState = ConsoleState {
                                       wSize   :: (Integer,Integer)
                                     , config  :: YMLConfig
                                     , charLUT :: M.Map Char [Int]
                                     , timeStr :: String
                                     , dateStr :: String
                                }

drawTimeString :: ConsoleState -> IO ()
drawTimeString state = do
                  let ConsoleState {wSize=_wSize, config=_config, charLUT=_charLUT, timeStr=_timeStr,dateStr=_dateStr} = state
                  let (mat,offs) = renderClock _config _timeStr  _wSize _charLUT
                  ANS.setCursorPosition  (snd offs) 0

                  forM_ mat $ \x->do 
                      ANS.setCursorColumn (fst offs)
                      putStrLn   x

                  ANS.setCursorColumn (getDateOffset mat offs _dateStr)
                  putStrLn _dateStr

updateClock :: StateT ConsoleState IO ()
updateClock = do
                cs@ConsoleState {wSize=_wSize, config=_config,  timeStr=_timeStr} <- get
                currentSize <- liftIO  getSize
                currentTime <- liftIO getZonedTime               

                let fmtTime str = formatTime defaultTimeLocale str currentTime
                let [time,date]  = [fmtTime (tstrf _config) | tstrf <- [timeDisplay,dateDisplay] ]

                let timeChanged = time /= _timeStr
                let winChanged  = currentSize /= _wSize
                 
                let currentState = cs{wSize=currentSize,timeStr=time, dateStr = date} 

                when winChanged $  liftIO ANS.clearScreen 
                when (timeChanged || winChanged) $ liftIO (drawTimeString currentState)

                liftIO $ threadDelay (updateDelay _config)
                updateClock

handleShutdown :: ThreadId -> IO ()
handleShutdown mainThreadID= do
                               ANS.showCursor
                               ANS.setCursorPosition 0 0
                               ANS.clearScreen
                               E.throwTo mainThreadID ExitSuccess
installHandlers :: IO Handler
installHandlers = do
                    tid <- myThreadId
                    installHandler sigINT (Catch $  handleShutdown tid) Nothing
                    installHandler sigTERM (Catch $ handleShutdown tid) Nothing

main :: IO ()
main = do
        dataPath <- getDataFileName "config.yml"
        cfg <- loadYML dataPath
        
        configureTerminal cfg         
        installHandlers

        evalStateT updateClock $ ConsoleState (0,0) cfg (getLUT cfg) "" ""
