module GE.GameRunner where

import qualified Data.HashMap.Strict as M

import GE.Types
import GE.Robot
import GE.MoveLogic
import GE.PathLogic
import GE.RoboEngine


gameServer :: GameConfig -> Coordinate -> MoveRes
gameServer gc c = case elem c (gcObstacles gc) of 
    True -> Blocked c
    _ | any (elem c) (gcGrid gc) -> Ok c
    _  -> Blocked c


gameStep :: Command -> Game -> Game
gameStep cmd g = case cmd of
  Move           -> g { gRobot = moveResHandler (gameServer gc (getRobotNextPos r)) r}
  SetDirection d -> g { gRobot = setRoboDir r d}
  where
    gc = gGameConfig g
    r  = gRobot g

gameAutoPlay :: Game -> Game
gameAutoPlay g = 
  case (gIsCompleted g) of
    True -> g
    False ->
      case getNextCommand red mr rob of
        (_,Done) ->  g { gIsCompleted = True }
        (red', UpdateEngineData) -> g {gRoboEngineData = red'}
        (red', cmd)              -> g { gRoboEngineData = red'
                                      , gLastResponse   = gameServerRes
                                      , gRobot          = newRobo'
                                      , gIsPathsConsistent = consistencyResult
                                      }
          where
            gameServerRes =
              case cmd of
                SetDirection d -> DirectionSet d 
                Move           -> (gameServer gConfig (getRobotNextPos rob))
            newRobo' = updateRoboPath rob $ newRobo
            newRobo = case cmd of
              SetDirection d -> setRoboDir rob d
              Move           -> moveResHandler gameServerRes rob
            consistencyResult = all checkConsistency . M.elems $ vpmPNOPPMap (rVisPortMeta newRobo')
  where
    red = gRoboEngineData g
    mr  = gLastResponse g
    rob = gRobot g
    gConfig = gGameConfig g
               
