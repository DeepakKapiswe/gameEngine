module GE.Robot where


import GE.Types


getRobotNextPos :: Robot -> Coordinate
getRobotNextPos (Robot (Coordinate x y) _ d _ _) = case d of
  UP    -> Coordinate x     (y+1) 
  RIGHT -> Coordinate (x+1) y
  DOWN  -> Coordinate x     (y-1)
  LEFT  -> Coordinate (x-1) y

getNeighbours :: Coordinate -> Neighbours
getNeighbours (Coordinate x y) =
  Neighbours 
   (Coordinate x     (y+1))
   (Coordinate (x+1) y    )
   (Coordinate x     (y-1))
   (Coordinate (x-1) y    )



setRoboDir :: Direction -> GameWorld -> GameWorld
setRoboDir d g = g {gwRobot = (gwRobot g){rDir = d}}