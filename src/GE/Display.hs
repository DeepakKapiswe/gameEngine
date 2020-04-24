module GE.Display where

import qualified Data.Map as M
import Data.List
import GE.Types

gameWorldToStr :: GameWorld -> String
gameWorldToStr (GameWorld rob@(Robot pos pm d visPoints blkPoints) (Grid points) obs) = gridStr
  where
    initialGrid = fmap (fmap coordToStr) points
    gridWithObs = foldl' (flip (changeWith obsStr)) initialGrid obs 
    gridWithVisitedPoints = M.foldlWithKey' changePointMeta gridWithObs visPoints  
    gridWithRobot = changeWith (robotToStr rob) pos gridWithVisitedPoints 
    gridStr = unlines . reverse . map concat $ gridWithRobot
    
    changeWith :: String -> Coordinate -> [[String]] -> [[String]]
    changeWith newStr (Coordinate x y) ls = front ++ replaced : back
      where
        front = take (y-1) ls
        back  = drop y ls
        toBeReplacedList = ls !! (y-1)
        replaced = (take (x-1) toBeReplacedList) ++ replacedStr : (drop x toBeReplacedList)
        toBeReplacedStr = (ls !! (y-1)) !! (x-1)
        replacedStr = (take 2 toBeReplacedStr) ++ newStr ++ (drop 4 toBeReplacedStr)
    
    changePointMeta :: [[String]] -> Coordinate -> PointMeta -> [[String]]
    changePointMeta ls (Coordinate x y) pm = front ++ replaced : back
      where
        front = take (y-1) ls
        back  = drop y ls
        toBeReplacedList = ls !! (y-1)
        replaced = (take (x-1) toBeReplacedList) ++ replacedStr : (drop x toBeReplacedList)
        toBeReplacedStr = (ls !! (y-1)) !! (x-1)
        replacedStr = pointMetaToStr pm

obsStr :: String
obsStr = "@@"

robotToStr :: Robot -> String
robotToStr (Robot _ _ d _ _) = case d of
    UP    -> "ß^"
    RIGHT -> "ß»"
    DOWN  -> "ßv"
    LEFT  -> "«ß"

pointMetaToStr :: PointMeta -> String
pointMetaToStr (PointMeta True True True True)     = "  ©   "
pointMetaToStr (PointMeta False True True True)    = " `©   " 
pointMetaToStr (PointMeta True False True True)    = "  ©  |" 
pointMetaToStr (PointMeta True True False True)    = "  © _ " 
pointMetaToStr (PointMeta True True True False)    = "| ©   " 
pointMetaToStr (PointMeta False False True True)   = " `©  |" 
pointMetaToStr (PointMeta True False False True)   = "  © _|" 
pointMetaToStr (PointMeta True True False False)   = "| © _ " 
pointMetaToStr (PointMeta False True True False)   = "|`©   " 
pointMetaToStr (PointMeta True False True False)   = "| ©  |" 
pointMetaToStr (PointMeta False False False True)  = " `© _|" 
pointMetaToStr (PointMeta True False False False)  = "| © _|" 
pointMetaToStr (PointMeta False True False False)  = "|`© _ " 
pointMetaToStr (PointMeta False True False True )  = " `© _ " 
pointMetaToStr (PointMeta False False True False)  = "|`©  |" 
pointMetaToStr (PointMeta False False False False) = "|`© _|" 

coordToStr :: Coordinate -> String
coordToStr = const "  •   "