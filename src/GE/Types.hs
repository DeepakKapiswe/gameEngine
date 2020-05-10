{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module GE.Types where

import Data.HashMap.Strict
import Data.Hashable
import GHC.Generics (Generic)
import Data.List (sortOn)


data Game = Game {
    gGameConfig     :: GameConfig
  , gRobot          :: Robot
  , gRoboEngineData :: RoboEngineData
  , gLastResponse   :: MoveRes
  , gIsCompleted    :: Bool
} deriving (Show, Eq)

data GameConfig = GameConfig {
    gcGrid :: [[Coordinate]]
  , gcObstacles :: [Coordinate]
} deriving (Show, Eq)


data RoboEngineData = RoboEngineData {
    redUExtreme     :: Coordinate
  , redRExtreme     :: Coordinate
  , redDExtreme     :: Coordinate
  , redLExtreme     :: Coordinate
  , redMMode        :: MMode
  , redNextCommands :: [Command]
  , redDestination  :: Maybe [Coordinate]
  , redLastMoveDirs :: [Direction]
} deriving (Show, Eq)

data MMode = NormalMode | TrappedMode
  deriving(Show, Eq)

data Robot = Robot {
    rPos            :: Coordinate
  , rPortMeta       :: PortMeta
  , rDir            :: Direction
  , rVisPortMeta    :: VisPortMeta
  , rBlockedCoords  :: [Coordinate]
} deriving (Show, Eq)

data VisPortMeta = VisPortMeta {
    vpmMaxPortNum    :: Int
  , vpmLastUpdatedPNs :: [Int]
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
} deriving (Eq, Ord, Generic)

instance Show Coordinate where
  show (Coordinate x y) = show x <> "≈" <> show y <> " | "

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
  | UpdateEngineData
  | Done
  deriving (Show, Eq)

data MoveRes = Ok Coordinate
             | Blocked Coordinate
             | DirectionSet Direction
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
  | InBlocked 
  | PortNotFound
  deriving (Show, Eq)