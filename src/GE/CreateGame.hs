module GE.CreateGame where

import GE.Types
import qualified Data.HashMap.Strict as M
import GE.PortMeta
import GE.VisPortMeta
import GE.RoboEngine


makeGrid :: Int -> Int -> [[Coordinate]]
makeGrid x y = reverse $ makeRow x <$> [1..y]
  where
    makeRow :: Int -> Int -> [Coordinate]
    makeRow x y = [Coordinate a y | a <- [1..x]] 

makeSampleGame :: Int -> Int -> Game
makeSampleGame x y = Game (GameConfig g sampleObs) r red res False
  where
    r = Robot cord initPortMeta LEFT visPortMeta []
    g = makeGrid x y 
    cord = Coordinate 5 5
    visPortMeta = insertPort cord initPortMeta $ initVisPorts 
    red = initRoboEngineData cord
    res = Ok cord
    -- sampleObs = []

sampleObs :: [Coordinate]
sampleObs = [
      Coordinate 3 1
    , Coordinate 3 4
    , Coordinate 4 4
    , Coordinate 3 6
    , Coordinate 2 4
    , Coordinate 7 1
    , Coordinate 3 5
    , Coordinate 6 5
    , Coordinate 9 2
    , Coordinate 3 3
    , Coordinate 6 3
    , Coordinate 4 8
    , Coordinate 7 7
    , Coordinate 5 3
    , Coordinate 8 7
    , Coordinate 2 9
    , Coordinate 6 3
    -- , Coordinate 12 8
    -- , Coordinate 13 7
    -- , Coordinate 5 13
    -- , Coordinate 11 7
    -- , Coordinate 12 9
    -- , Coordinate 5 13
    -- , Coordinate 11 7
    -- , Coordinate 1 7
    -- , Coordinate 11 7 --
    -- , Coordinate 12 14
    -- , Coordinate 14 7
    -- , Coordinate 14 1
    -- , Coordinate 15 1
    ]
