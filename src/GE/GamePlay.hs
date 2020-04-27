module GE.GamePlay where


import qualified Data.Map as M

import GE.Types


runRobo :: GameWorld -> GameWorld
runRobo gw@(GameWorld r@(Robot currPos cPM d vPts bPts) g@(Grid points) obs) =
  case moveReq gw of
    Right x -> x
    Left (MoveStopNoti nextPos)  -> GameWorld updatedRobo (Grid points) obs
      where
        updatedRobo =  Robot currPos roboNewPm d newVisPoints newBlkPoints
        roboNewPm = setPM d cPM
        newVisPoints = M.insert currPos roboNewPm vPts
        newBlkPoints = nextPos : bPts
moveReq :: GameWorld -> Either MoveStopNoti GameWorld
moveReq (GameWorld r@(Robot currPos cPM d vPts bPts) g@(Grid points) obs) 
  | elem nextPos obs = Left $ MoveStopNoti nextPos 
  | notElem nextPos (concat points) = Left $ MoveStopNoti nextPos
  | otherwise = Right $ GameWorld updatedRobot g obs 
  where
    nextPos = getRobotNextPos r
    updatedRobot = Robot nextPos updRobotPm  d updVisPortMap bPts
    
    newRobotPm = case M.lookup nextPos vPts of
        Just newPm -> newPm
        _          -> initPortMeta
    
    (setPmList', vPts')  = case (updateNeighbour uNbr pUOpen setDOpen vPts) of
      Nothing -> ([],vPts)
      Just newMap -> ([UP],newMap)
    
    (setPmList'', vPts'')  = case (updateNeighbour rNbr pROpen setLOpen vPts') of
      Nothing -> (setPmList',vPts')
      Just newMap -> (RIGHT : setPmList', newMap)
    
    (setPmList''', vPts''')  = case (updateNeighbour dNbr pDOpen setUOpen vPts'') of
      Nothing -> (setPmList'',vPts'')
      Just newMap -> (DOWN : setPmList'', newMap)
    
    (setPmList'''', vPts'''')  = case (updateNeighbour lNbr pLOpen setROpen vPts''') of
      Nothing -> (setPmList''',vPts''')
      Just newMap -> ( LEFT : setPmList''', newMap)

    updateRobotPm :: PortMeta -> [Direction] -> PortMeta
    updateRobotPm  pm [] = pm
    updateRobotPm pm (d:ds) =
      let pm' = case d of 
                  UP    -> setUOpen pm  
                  RIGHT -> setROpen pm
                  DOWN  -> setDOpen pm
                  LEFT  -> setLOpen pm
      in updateRobotPm pm' ds
    
    updRobotPm     = updateRobotPm newRobotPm setPmList''''
    updVisPortMap = M.insert nextPos updRobotPm  vPts'''' 
    
    
    
    updateNeighbour             
      :: Coordinate                 -- Coordinate of Adjacent Neighbour
      -> (PortMeta -> Bool)        -- Which PortMeta to Look
      -> (PortMeta -> PortMeta)   -- How to update Pm of Neighbour
      -> PortMap                   -- Visited Point Map
      -> Maybe PortMap             -- Updated Point Map If Neighbour Visited 
    updateNeighbour nbr pmgf pmUpdf pMap = case pmgf newRobotPm of
      False -> Nothing  -- It means earlier visited from this direction, no need to update Map
      True  -> case M.updateLookupWithKey  (updF pmUpdf) nbr pMap of
        (Nothing, _) -> Nothing
        (_, newPMap) -> Just newPMap
          
    
    nbrs = getNeighbours nextPos
    uNbr = nUp    nbrs  
    rNbr = nRight nbrs  
    dNbr = nDown  nbrs  
    lNbr = nLeft  nbrs

    updF :: (PortMeta -> PortMeta) -> Coordinate -> PortMeta -> Maybe PortMeta 
    updF  f _ pm = Just $ f pm


setUOpen :: PortMeta -> PortMeta
setUOpen p = p {pUOpen = False}

setROpen :: PortMeta -> PortMeta
setROpen p = p {pROpen = False}

setDOpen :: PortMeta -> PortMeta
setDOpen p = p {pDOpen = False}

setLOpen :: PortMeta -> PortMeta
setLOpen p = p {pLOpen = False}

setPM :: Direction -> PortMeta -> PortMeta
setPM d = case d of
  UP    -> setUOpen 
  RIGHT -> setROpen 
  DOWN  -> setDOpen 
  LEFT  -> setLOpen

    
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
 
initPortMeta :: PortMeta
initPortMeta = PortMeta True True True True True False 


setRoboDir :: Direction -> GameWorld -> GameWorld
setRoboDir d g = g {gwRobot = (gwRobot g){rDir = d}}