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
  , rPointPortMeta :: PortMeta
  , rDir           :: Direction
  , rVistedPoints  :: Map Coordinate PortMeta
  -- , rVisOpenPorts  :: Map Coordinate PortMeta
  , rBlockedCoords :: [Coordinate]
} deriving (Show, Eq)

type PortMap = Map Coordinate PortMeta

data Coordinate = Coordinate {
    cX :: Int
  , cY :: Int
} deriving (Show, Eq, Ord)


data PortMeta = PortMeta {
    pUOpen        :: Bool
  , pROpen        :: Bool
  , pDOpen        :: Bool
  , pLOpen        :: Bool
  , pIsFirstVisit :: Bool
  , pIsClosed     :: Bool
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