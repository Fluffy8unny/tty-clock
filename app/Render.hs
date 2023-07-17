module Render (renderClock) where

import Data.Bits ( Bits((.&.), zeroBits, bit) ) 
import Data.List (transpose,intercalate)
import qualified Data.Map as M

import Config
import Data.Yaml.Builder (string)

renderMatrix::(Ord a,Bits b,Integral c)=>[a]->c->M.Map a [b]->[[Bool]]
renderMatrix str numBits lut = let 
                                    valueMatrix    = transpose  [lut M.! c | c <- str] 
                                    parseBits bits = (/=zeroBits) <$> [ bits  .&. bit (fromIntegral i)  | i <-[0..numBits-1]]  
                                    drawBits       = concatMap parseBits 
                                in
                                    map drawBits valueMatrix

renderString :: YMLConfig -> [[Bool]] -> [[Char]]
renderString YMLConfig{symbolOn=sOn,symbolOff=sOff} =  map (map (\b-> if b then sOn else sOff ))

getMatrixShape::[[a]]->(Int,Int)
getMatrixShape matrix = ((length.head) matrix, length matrix)

calculateOffset::(Integral b) => [[a]] -> YMLConfig -> (b,b) -> (b,b)
calculateOffset matrix cfg screenSize = let
                                       calcVal dim size cfg = case cfg of
                                                                CENTER -> (dim -  size ) `div` 2
                                                                (FIXED i) -> fromIntegral i
                                       (stringWidth,stringHeight)  = getMatrixShape matrix
                                       offsetX = calcVal (fst screenSize) (fromIntegral stringWidth)  (centerX cfg)
                                       offsetY = calcVal (snd screenSize) (fromIntegral stringHeight) (centerY cfg)
                                    in
                                       ( offsetX, offsetY )

centerClock :: Integral a => [[Char]] -> (a, a) -> YMLConfig -> [[Char]]
centerClock matrix (offsetX,offsetY) cfg = let
                                            marginX = replicate (fromIntegral offsetX) (symbolOff cfg) 
                                            marginY = replicate (fromIntegral offsetY) "\n" 
                                            horizontalCentered = map ( \row -> marginX ++ row ++ marginX ) matrix
                                        in
                                            marginY ++ horizontalCentered ++ marginY


calcZoomFactor :: (Foldable t,Integral b) => [t a] -> (b, b) -> b -> Int
calcZoomFactor matrix screenSize border= let
                                            calMaxFactor s s' = (s' - border) `div` s 
                                            matSize = ( (fromIntegral.length.head) matrix,(fromIntegral.length) matrix)
                                        in
                                            max 0 $ minimum $ (\f-> fromIntegral $ calMaxFactor (f matSize) (f screenSize)) <$> [fst,snd]             
applyZoom::Int->[[a]]->[[a]]
applyZoom factor arr = let
                          repl  = concatMap (replicate factor)
                       in
                          repl ( map repl arr  )

getZoomFactor::(Integral b)=>[[a]]->(b,b)->YMLConfig->Int
getZoomFactor matrix screenSize cfg@YMLConfig{zoom=z} = case z of
                                                 INT fixedZoom -> fixedZoom
                                                 ZOOM_AUTO     -> fromIntegral $ calcZoomFactor matrix screenSize ((fromIntegral.border) cfg)     
                                                 ZOOM_OFF      -> 1 

matrixToString :: [[Char]] -> [Char]
matrixToString = intercalate "\n" 

renderClock :: (Ord a, Bits b1, Integral b2) =>YMLConfig -> [a] -> (b2, b2) -> M.Map a [b1] -> ([Char],(Int,Int))
renderClock cfg time windowSize lut = let
                                        clockString    = renderString cfg $ renderMatrix time (glyphWidth cfg + 1) lut

                                        zoomFactor     = getZoomFactor clockString windowSize cfg
                                        zoomedString   = applyZoom zoomFactor clockString

                                        offset         = calculateOffset zoomedString cfg windowSize
                                        centeredMatrix = centerClock zoomedString offset cfg
                                     in
                                        (matrixToString centeredMatrix, getMatrixShape centeredMatrix)