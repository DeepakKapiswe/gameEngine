{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module GE.Types where

import Data.HashMap.Strict
import Data.Hashable
import GHC.Generics (Generic)

data GameWorld = GameWorld {
    gwRobot        :: Robot
  , gwGrid         :: Grid
  , gwObstacles    :: [Coordinate]
} deriving (Show, Eq)

data Robot = Robot {
    rPos            :: Coordinate
  , rPortMeta       :: PortMeta
  , rDir            :: Direction
  , rVisPortMeta    :: VisPortMeta
  , rBlockedCoords  :: [Coordinate]
} deriving (Show, Eq)

data VisPortMeta = VisPortMeta {
    vpmMaxPortNum    :: Int
  , vpmLastUpdatedPN :: Int
  -- , vpmLastInterval  :: PortInterval
  , vpmCPorts        :: Map Coordinate PortMeta
  , vpmOPorts        :: Map Coordinate PortMeta
  , vpmCoordPNMap    :: Map Coordinate PortNum
  , vpmPNOPPMap      :: Map PortNum OpenPortPath
  -- , vpmIntervalPNMap :: Map PortInterval PortInterval
} deriving (Show, Eq)

type PortNum = Int
type Map = HashMap

type OpenPortPath = [Coordinate]



data Coordinate = Coordinate {
    cX :: Int
  , cY :: Int
} deriving (Show, Eq, Ord, Generic)

instance Hashable Coordinate


data PortMeta = PortMeta {
    pUOpen        :: Bool
  , pROpen        :: Bool
  , pDOpen        :: Bool
  , pLOpen        :: Bool
  , pIsFirstVisit :: Bool
  , pIsClosed     :: Bool
} deriving (Show, Eq)

type Grid = [[Coordinate]]
  -- deriving (Show, Eq)

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

data MoveRes = Ok Coordinate
             | Blocked Coordinate
  deriving (Show, Eq)

data PortInterval = PortInterval {
    piLow  :: PortNum
  , piHigh :: PortNum
  } deriving (Show, Eq, Generic)

instance Hashable PortInterval

instance Ord PortInterval where
  compare (PortInterval x _) (PortInterval l h)
    | x < l = LT
    | x > h = GT
    | otherwise = EQ