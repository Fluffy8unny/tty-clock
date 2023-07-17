module Main where

import Data.Time (formatTime,getZonedTime,defaultTimeLocale)
import qualified System.Console.Terminal.Size as T

import Config
import Render ( renderClock )


getTime::YMLConfig->IO String
getTime cfg = formatTime defaultTimeLocale (timeDisplay cfg) <$> getZonedTime

getSize::IO (Integer,Integer)
getSize = do
           size <- T.size
           case size of
             Just T.Window{T.height=h, T.width=w} -> return (w,h)
             Nothing                              -> error "Couldn't get window size"

main :: IO ()
main = do
        cfg <- loadYML "config.yml"
        let lut = getLUT cfg
        
        time <- getTime cfg
        windowSize <- getSize

        let (mat,shape) = renderClock cfg time  windowSize lut
        putStrLn mat