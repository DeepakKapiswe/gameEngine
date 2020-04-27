module GE.CreateGame where

import Data.Map
import GE.Types
import GE.GamePlay



makeGrid :: Int -> Int -> Grid
makeGrid x y = Grid $ reverse $ makeRow x <$> [1..y]
  where
    makeRow :: Int -> Int -> [Coordinate]
    makeRow x y = [Coordinate a y | a <- [1..x]] 

makeSampleGameWorld :: Int -> Int -> GameWorld
makeSampleGameWorld x y = GameWorld r g sampleObs
  where
    r = Robot cord initPointMeta UP (insert cord initPointMeta  mempty) []
    g = makeGrid x y 
    cord = Coordinate 5 5

sampleObs :: [Coordinate]
sampleObs = [
      Coordinate 3 1
    , Coordinate 3 4
    , Coordinate 4 4
    , Coordinate 3 6
    -- , Coordinate 2 4
    , Coordinate 7 1
    , Coordinate 3 5
    , Coordinate 6 5
    , Coordinate 8 7
    -- , Coordinate 9 2
    -- , Coordinate 3 3
    -- , Coordinate 6 3
    -- , Coordinate 4 8
    , Coordinate 7 7
    , Coordinate 5 3
    , Coordinate 8 7
    , Coordinate 2 9
     
    ]

s = 's'
b = 's'
v = 's'
