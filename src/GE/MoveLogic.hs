module GE.MoveLogic where


import qualified Data.HashMap.Strict as M
import Data.List (foldl')

import GE.Types
import GE.PortMeta
import GE.VisPortMeta
import GE.Robot
import GE.PathLogic



-- | takes a Move Response and changes the Robot Metadata accordingly
moveResHandler :: MoveRes -> Robot -> Robot
moveResHandler mRes r@(Robot currPos cPM d visPM bPts) =
  case mRes of
    Ok nextPos -> validMoveResHandler nextPos r
    Blocked nextPos  -> blockedMoveResHandler nextPos r

blockedMoveResHandler :: Coordinate -> Robot -> Robot
blockedMoveResHandler nextPos r@(Robot currPos cPM d _ bPts) = 
  updatedRobo
  where
    updatedRobo =  r { rPortMeta = roboNewPm
                     , rBlockedCoords = newBlkPoints
                     , rVisPortMeta = roboNewVisPm }
    roboNewPm    = setPM d cPM
    roboNewVisPm = updatePort currPos (setPM d) (rVisPortMeta r)
    newBlkPoints = nextPos : bPts

validMoveResHandler :: Coordinate -> Robot -> Robot
validMoveResHandler nextPos r@(Robot lastPos cPM d visPM _) = 
  case M.lookup nextPos (vpmCPorts visPM) of
    Just newPm ->  newRobo                                  -- this means is the newPos is a Completed Port
      where
        newRobo = r { rPos = nextPos
                    , rPortMeta = newPm}  
    Nothing -> case M.lookup nextPos (vpmOPorts visPM) of
      Nothing -> newRobo                                    -- this means nextPos is never visited before
        where
          newRobo  = lUpdRobo { rPortMeta = newRoboPm
                              , rVisPortMeta = newRoboVisPm 
                              }

          newRoboPm    = rPortMeta lUpdRobo
          newRoboVisPm = insertPort nextPos newRoboPm $ rVisPortMeta lUpdRobo 
          
          r' = r { rPos = nextPos
                 , rPortMeta = initPortMeta }
          nbrs = getNeighbours nextPos
          uNbr = nUp    nbrs  
          rNbr = nRight nbrs  
          dNbr = nDown  nbrs  
          lNbr = nLeft  nbrs
          
          uUpdRobo = 
            case M.lookup uNbr (vpmOPorts $ rVisPortMeta r') of
              Nothing -> r'
              Just uNbrPm -> roboAndUpNeigbourUpdated
                where
                  oldVisPm = rVisPortMeta r'
                  newRoboPm = setUOpen (rPortMeta r')
                  newRoboVisPm = updatePort uNbr setDOpen $ rVisPortMeta r'
                  roboAndUpNeigbourUpdated = r' {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
          rUpdRobo = 
            case M.lookup rNbr (vpmOPorts $ rVisPortMeta uUpdRobo) of
              Nothing -> uUpdRobo
              Just rNbrPm -> roboAndRightNeigbourUpdated
                where
                  newRoboPm = setROpen (rPortMeta uUpdRobo)
                  newRoboVisPm = updatePort rNbr setLOpen $ rVisPortMeta uUpdRobo
                  roboAndRightNeigbourUpdated = uUpdRobo {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
          dUpdRobo = 
            case M.lookup dNbr (vpmOPorts $ rVisPortMeta rUpdRobo) of
              Nothing -> rUpdRobo
              Just dNbrPm -> roboAndDownNeigbourUpdated
                where
                  newRoboPm = setDOpen (rPortMeta rUpdRobo)
                  newRoboVisPm = updatePort dNbr setUOpen $ rVisPortMeta rUpdRobo 
                  roboAndDownNeigbourUpdated = rUpdRobo {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
          lUpdRobo = 
            case M.lookup lNbr (vpmOPorts $ rVisPortMeta dUpdRobo) of
              Nothing -> dUpdRobo
              Just lNbrPm -> roboAndLeftNeigbourUpdated
                where
                  newRoboPm = setLOpen (rPortMeta dUpdRobo)
                  newRoboVisPm = updatePort lNbr setROpen $ rVisPortMeta dUpdRobo
                  roboAndLeftNeigbourUpdated = dUpdRobo {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}


      Just newPM -> newRobo                                    -- this means nextPos is never visited before
        where
          newRobo  = lUpdRobo { rPortMeta = newRoboPm
                              , rVisPortMeta = newRoboVisPm 
                              }
  
  
          newRoboPm    = rPortMeta lUpdRobo
          newRoboVisPm = updatePort nextPos (const newRoboPm) neighbourUpdVisPm
          neighbourUpdVisPm = rVisPortMeta lUpdRobo 
          
          r' = r { rPos = nextPos
                 , rPortMeta = newPM }
          nbrs = getNeighbours nextPos
          uNbr = nUp    nbrs  
          rNbr = nRight nbrs  
          dNbr = nDown  nbrs  
          lNbr = nLeft  nbrs
          
          uUpdRobo | (not $ pUOpen newPM) = r'
                   | otherwise = 
            case M.lookup uNbr (vpmOPorts $ rVisPortMeta r') of
              Nothing -> r'
              Just uNbrPm -> roboAndUpNeigbourUpdated
                where
                  newRoboPm = setUOpen (rPortMeta r')
                  newRoboVisPm = updatePort uNbr setDOpen $ rVisPortMeta r'
                  roboAndUpNeigbourUpdated = r' {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
          rUpdRobo = 
            case M.lookup rNbr (vpmOPorts $ rVisPortMeta uUpdRobo) of
              Nothing -> uUpdRobo
              Just rNbrPm -> roboAndRightNeigbourUpdated
                where
                  newRoboPm = setROpen (rPortMeta uUpdRobo)
                  newRoboVisPm = updatePort rNbr setLOpen $ rVisPortMeta uUpdRobo
                  roboAndRightNeigbourUpdated = uUpdRobo {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
          dUpdRobo = 
            case M.lookup dNbr (vpmOPorts $ rVisPortMeta rUpdRobo) of
              Nothing -> rUpdRobo
              Just dNbrPm -> roboAndDownNeigbourUpdated
                where
                  newRoboPm = setDOpen (rPortMeta rUpdRobo)
                  newRoboVisPm = updatePort dNbr setUOpen $ rVisPortMeta rUpdRobo 
                  roboAndDownNeigbourUpdated = rUpdRobo {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
          lUpdRobo = 
            case M.lookup lNbr (vpmOPorts $ rVisPortMeta dUpdRobo) of
              Nothing -> dUpdRobo
              Just lNbrPm -> roboAndLeftNeigbourUpdated
                where
                  newRoboPm = setLOpen (rPortMeta dUpdRobo)
                  newRoboVisPm = updatePort lNbr setROpen $ rVisPortMeta dUpdRobo
                  roboAndLeftNeigbourUpdated = dUpdRobo {rPortMeta = newRoboPm , rVisPortMeta = newRoboVisPm}
