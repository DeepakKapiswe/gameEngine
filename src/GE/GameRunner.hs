module GE.GameRunner where


import GE.Types
import GE.Robot
import GE.MoveLogic


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

