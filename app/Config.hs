{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module Config (
                loadYML
              , getLUT
              , YMLConfig( .. )
              , ZoomSetting(..)
              , CenteringSettings (..)
              , CharAsBin( .. )
) where

import Data.Yaml
import qualified Data.Text as T
import qualified Data.Scientific as S
import GHC.Generics ( Generic )
import qualified Data.Map as M

data CharAsBin = CharAsBin{  char :: Char
                           , bits :: [Int]
                          } deriving (Show, Generic)
instance FromJSON CharAsBin


data ZoomSetting = INT Int| ZOOM_AUTO | ZOOM_OFF deriving (Show)
instance FromJSON ZoomSetting where
    parseJSON :: Value -> Parser ZoomSetting
    parseJSON (String i)   | i == T.pack "auto" =  return ZOOM_AUTO
                           | i == T.pack "off"  =  return ZOOM_OFF
    parseJSON (Number i) = case S.toBoundedInteger i of
                               Just i -> return (INT i)
                               Nothing -> error "zoom can be either an Integer,'off' or 'auto'"
    parseJSON _          = error "zoom can be either an Integer,'off' or 'auto'"

data CenteringSettings  = FIXED Int | CENTER deriving (Show)
instance FromJSON CenteringSettings where
    parseJSON :: Value -> Parser CenteringSettings
    parseJSON (String i) | i == T.pack "center" = return CENTER
                         | i == T.pack "off"    = return (FIXED 0)
    parseJSON (Number i) = case S.toBoundedInteger i of
                               Just i -> return (FIXED i)
                               Nothing -> error "centering needs to be either 'center', 'off' or an integer"
    parseJSON _          = error "centering needs to be either 'center','off' or an integer"

data YMLConfig  = YMLConfig{     glyphs      :: [CharAsBin]
                               , glyphHeight :: Int
                               , glyphWidth  :: Int
                               , symbolOff   :: Char
                               , symbolOn    :: Char
                               , timeDisplay :: String
                               , border      :: Int
                               , zoom        :: ZoomSetting
                               , centerX     :: CenteringSettings
                               , centerY     :: CenteringSettings
                     } deriving (Show,Generic)
instance FromJSON YMLConfig


verifyConfig :: YMLConfig -> YMLConfig
verifyConfig cfg = let
                      heightOk             = map ((== glyphHeight cfg) . length . bits) (glyphs cfg)
                      widthOK              = map (all (<( 2 ^ glyphWidth cfg )).bits) (glyphs cfg)             
                      checkError errs msg = concatMap (\x-> msg ++ (show.snd) x) $ filter (not.fst) $ zipWith (,) errs (glyphs cfg) 
                      errMsgWidth  = checkError widthOK  "Width of glyph is not equal to glyphWidth. Glyph:"
                      errMsgHeight = checkError heightOk "Byte in glyph is out of bounds for glpyHeight. Should be < 2^glyphHeight. Glyph:"
                   in 
                      case all (==True) $ widthOK ++ heightOk of
                        True -> cfg
                        False -> error (errMsgHeight++errMsgWidth)

loadYML :: FilePath -> IO YMLConfig
loadYML path = do
                file <- decodeFileEither path :: IO (Either ParseException YMLConfig)
                case file of 
                  Right val -> return (verifyConfig val)
                  Left err -> error (prettyPrintParseException err)

getLUT::YMLConfig -> M.Map Char [Int]
getLUT cfg = M.fromList [(char g, bits g ) | g <- glyphs cfg]
