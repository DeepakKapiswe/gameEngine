{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module GE.Types where

import Data.HashMap.Strict
import Data.Hashable
import GHC.Generics (Generic)


data Game = Game {
    gGameConfig :: GameConfig
  , gRobot      :: Robot
} deriving (Show, Eq)

data GameWorld = GameWorld {
    gwRobot        :: Robot
  , gwGrid         :: Grid
  , gwObstacles    :: [Coordinate]
} deriving (Show, Eq)

data GameConfig = GameConfig {
    gcGrid :: [[Coordinate]]
  , gcObstacles :: [Coordinate]
} deriving (Show, Eq)

data Robot = Robot {
    rPos            :: Coordinate
  , rPortMeta       :: PortMeta
  , rDir            :: Direction
  , rVisPortMeta    :: VisPortMeta
  , rBlockedCoords  :: [Coordinate]
} deriving (Eq)

instance Show Robot where
  show (Robot pos pm _ pmMap _) = unlines [show pos, show pm, show pmMap]

data VisPortMeta = VisPortMeta {
    vpmMaxPortNum    :: Int
  , vpmLastUpdatedPNs :: [Int]
  -- , vpmLastInterval  :: PortInterval
  , vpmCPorts        :: Map Coordinate PortMeta
  , vpmOPorts        :: Map Coordinate PortMeta
  , vpmCoordPNMap    :: Map Coordinate PortNum
  , vpmPNOPPMap      :: Map PortNum OpenPortPath
  -- , vpmIntervalPNMap :: Map PortInterval PortInterval
} deriving (Eq)

instance Show VisPortMeta where
  show visPM@(VisPortMeta vpmMax vpmLast vCPorts vOPorts vCPNMap vPNOPPMap) =
    unlines [
        "vpmMax: " <> show vpmMax
      , "\nvpmLast: \n" <> show vpmLast
      , "\nClosedPorts: \n" <> (unlines $ fmap show $ toList vCPorts )
      , "\nOpenPorts: \n" <> (unlines $ fmap show $ toList vOPorts )
      , "\nC -> PN : \n" <> (unlines $ fmap show $ toList vCPNMap )
      , "\nPN -> Paths: \n" <> (unlines $ fmap show $ toList vPNOPPMap )
    ]  

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

data PortLookupResult =
    InOpen PortMeta
  | InClosed PortMeta
  | PortNotFound
  deriving (Show, Eq)