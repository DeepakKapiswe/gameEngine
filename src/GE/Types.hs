{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module GE.Types where

import Data.Map
import GHC.Generics (Generic)

data GameWorld = GameWorld {
    gwRobot        :: Robot
  , gwGrid         :: Grid
  , gwObstacles    :: [Coordinate]
} deriving (Show, Eq)


data Robot = Robot {
    rPos           :: Coordinate
  , rPointMeta     :: PointMeta
  , rDir           :: Direction
  , rVistedPoints  :: Map Coordinate PointMeta
  , rBlockedCoords :: [Coordinate]
} deriving (Show, Eq)

type PointMap = Map Coordinate PointMeta


data Coordinate = Coordinate {
    cX :: Int
  , cY :: Int
} deriving (Show, Eq, Ord)


data PointMeta = PointMeta {
    pUOpen :: Bool
  , pROpen :: Bool
  , pDOpen :: Bool
  , pLOpen :: Bool
} deriving (Show, Eq)

newtype Grid = Grid [[Coordinate]]
  deriving (Show, Eq)

data Direction =
    UP
  | RIGHT
  | DOWN
  | LEFT
  deriving (Show, Eq, Enum, Generic)

data MoveStopNoti = MoveStopNoti Coordinate
  deriving (Show, Eq)

data Neighbours = Neighbours {
    nUp    :: Coordinate
  , nRight :: Coordinate
  , nDown  :: Coordinate
  , nLeft  :: Coordinate
} deriving (Show, Eq)

data Command = 
    Move 
  | SetDirection Direction 
  deriving (Show, Eq)