module Menu
where

 import Graphics.Gloss.Interface.Pure.Game
 import Data.Array
 

 modPvPcolor = orange  
 modPvEcolor = magenta
 menuSpace = blue



 menuSp::Picture
 menuSp = blank


 menuPlayPic menu = 
                (pictures [translate (fromIntegral 750*(0.5)) (fromIntegral 800*(0.5)) (color modPvPcolor $ buttomPic) 
                 ,color menuSpace menuSp,translate (fromIntegral 750*(0.5)) (fromIntegral 350*(0.5)) (color modPvEcolor $ buttomPic)
                 ,translate (fromIntegral 600*(0.5)) (fromIntegral 320*(0.5)) $ scale (0.6) (0.3) $ text "PvE"
                 ,translate (fromIntegral 599*(0.5)) (fromIntegral 321*(0.5)) $ scale (0.6) (0.3) $ text "PvE"
                 ,translate (fromIntegral 598*(0.5)) (fromIntegral 322*(0.5)) $ scale (0.6) (0.3) $ text "PvE"
                 ,translate (fromIntegral 596*(0.5)) (fromIntegral 324*(0.5)) $ scale (0.6) (0.3) $ text "PvE"

                 ,translate (fromIntegral 600*(0.5)) (fromIntegral 780*(0.5)) $ scale (0.6) (0.3) $ text "PvP"
                 ,translate (fromIntegral 599*(0.5)) (fromIntegral 781*(0.5)) $ scale (0.6) (0.3) $ text "PvP"
                 ,translate (fromIntegral 598*(0.5)) (fromIntegral 782*(0.5)) $ scale (0.6) (0.3) $ text "PvP"
                 ,translate (fromIntegral 596*(0.5)) (fromIntegral 784*(0.5)) $ scale (0.6) (0.3) $ text "PvP"
                 ])
  
                             

 buttomPos :: Float
 buttomPos = 400.0

 buttomSize :: Float
 buttomSize = 100.0 

 buttomPic::Picture
 buttomPic = rectangleSolid buttomPos buttomSize 

 

 


 

