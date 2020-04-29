module GE.MoveLogic where

-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as M
import Data.List (foldl')

import GE.Types
import GE.PortMeta
import GE.Robot


-- | takes a Move Response and changes the Robot Metadata accordingly
moveResHandler :: MoveRes -> Robot -> Robot
moveResHandler mRes r@(Robot currPos cPM d visPM bPts) =
  case mRes of
    Ok nextPos -> validMoveResHandler nextPos r
    Blocked nextPos  -> blockedMoveResHandler nextPos r

blockedMoveResHandler :: Coordinate -> Robot -> Robot
blockedMoveResHandler nextPos r@(Robot _ cPM d _ bPts) = updatedRobo
  where
    updatedRobo =  r { rPortMeta = roboNewPm, rBlockedCoords = newBlkPoints}
    roboNewPm = setPM d cPM
    newBlkPoints = nextPos : bPts

validMoveResHandler :: Coordinate -> Robot -> Robot
validMoveResHandler nextPos r@(Robot lastPos cPM d visPM _) =
  case M.lookup nextPos (vpmCPorts visPM) of
    Just newPm -> r'
      where
        r' = r { rPos = nextPos
               , rPortMeta = newPm
               }
    Nothing -> case M.lookup nextPos (vpmOPorts visPM) of
      Nothing -> r { rPos = nextPos
                   , rPortMeta = initPortMeta 
                   , rVisPortMeta = visPM {
                       vpmOPorts = M.insert nextPos initPortMeta (vpmOPorts visPM)
                       }
                   }

      Just newPm -> updatedRobo 
        where
          
          nbrs = getNeighbours nextPos
          uNbr = nUp    nbrs  
          rNbr = nRight nbrs  
          dNbr = nDown  nbrs  
          lNbr = nLeft  nbrs
          
          updatedRobo = 
            updateNeighbour 
              (pUOpen cPM) 
              uNbr
              setDOpen
              setUOpen
            $ updateNeighbour 
              (pROpen cPM) 
              rNbr
              setLOpen
              setROpen
            $ updateNeighbour 
              (pDOpen cPM) 
              dNbr
              setUOpen
              setDOpen
            $ updateNeighbour 
              (pLOpen cPM) 
              lNbr
              setROpen
              setLOpen
              r

updatePaths 
  :: Coordinate               -- The Coordinate of the last position of robot
  -> PortMeta                 -- Updated PortMeta of last position of robot from where it came
  -> VisPortMeta              -- Visited Port Meta to update
  -> (PortMeta, VisPortMeta)  -- Updated Visited Port Meta with updated paths
updatePaths
  lastCord
  lastPM
  visPM@(VisPortMeta vpmMax vpmLast vpmLInt _ _ vCPNMap vPNOPPMap vIntPNMap ) =
    case pIsFirstVisit lastPM  of
      True -> (pm', visPM')    -- we can be sure this is the first time robot
                               -- visited this port. So just insert a new OPP
                               -- and update all previous without any check
                               -- and set the Fisrt Visit flag to False
        where
          visPM'     = visPM {
              vpmMaxPortNum    = vpmMax'
            , vpmLastUpdatedPN = vpmMax
            , vpmLastInterval  = vpmLInt'
            , vpmCoordPNMap    = vCPNMap'
            , vpmPNOPPMap      = vPNOPPMap'
            , vpmIntervalPNMap = vIntPNMap'
          }
          vpmMax'    = succ vpmMax 
          vCPNMap'   = M.insert lastCord vpmMax vCPNMap
          vPNOPPMap' = M.insert vpmMax' [lastCord] $ fmap ( lastCord : ) vPNOPPMap   
          pm'        = lastPM { pIsFirstVisit = False }
          (vpmLInt', vIntPNMap') = updateLastInterval vpmLInt vIntPNMap

      False -> case pIsClosed lastPM of
        True  -> (lastPM, visPM )  -- Do Nothing as we can be sure whenever robot travels through
                                   -- a Closed port then its destination must be an open port
                                   -- and also we can be sure that the robot will must reach its
                                   -- destination so there is no need to update the paths in between
                                   -- as all the effect will be washed away when it will reach the
                                   -- destination open port which was in history  
        False -> a
    where
      updateLastInterval 
        :: PortInterval                    -- The last portInterval to update
        -> Map PortInterval PortInterval  -- The Map to update
        -> (PortInterval, Map PortInterval PortInterval)   -- returns the updated Map
      updateLastInterval pi piMap =
        let pi' = pi {piHigh = (piHigh pi) + 1}
        in  (pi', M.insert pi' pi' $ M.delete pi piMap)




  


updateNeighbour
  :: Bool                    -- do not update if already neighbour is closed
  -> Coordinate              -- coordinate of the neighbour
  -> (PortMeta -> PortMeta)  -- how to update the portMeta of Neighbour
  -> (PortMeta -> PortMeta)  -- how to update the portMeta of Robot
  -> Robot                   -- current robot
  -> Robot                   -- updated robot
updateNeighbour
  portOpen
  c@(Coordinate x y)          
  pmsfN                      -- portMeta Setter Function for Neighbour
  pmsfR
  r@(Robot currPos cPM d visPM _)
    | not portOpen = r
    | otherwise =
  case M.lookup c (vpmOPorts visPM) of
    Nothing -> r
    Just nbrPm -> r {
        rPortMeta    = updatedRoboPm
      , rVisPortMeta = updatedVisPortMeta
    }
      where
        updatedNbrPm  = pmsfN nbrPm
        updatedRoboPm = pmsfR cPM
        updatedVisPortMeta = case pIsClosed updatedNbrPm of
          False -> let nUpdatedOPorts = M.insert c updatedNbrPm (vpmOPorts visPM)
                   in visPM {vpmOPorts = nUpdatedOPorts}
          True  -> portCloseHandler c updatedNbrPm visPM

-- | This function should be called whenever a port is closed
portCloseHandler 
  :: Coordinate
  -> PortMeta 
  -> VisPortMeta
  -> VisPortMeta
portCloseHandler 
  cord
  pm
  vpm@(VisPortMeta _ _ _ visCPorts visOPorts cordPNMap pnOPPMap intPNMap) = 
    vpm {
      vpmLastUpdatedPN  = portNum
    , vpmCPorts         = vpmCPorts'
    , vpmOPorts         = vpmOPorts'
    , vpmCoordPNMap     = vpmCoordPNMap'
    , vpmPNOPPMap       = vpmPNOPPMap'
    , vpmIntervalPNMap  = vpmIntervalPNMap'
        }
  where
    portNum        = cordPNMap M.! cord
    vpmCPorts'     = M.insert cord pm visCPorts
    vpmOPorts'     = M.delete cord visOPorts
    vpmCoordPNMap' = M.delete cord cordPNMap
    vpmPNOPPMap'   = foldl' 
                       (flip $ M.adjust (dropWhile (/= cord)))  
                       (M.delete portNum pnOPPMap) 
                       (intervalToPNs newInt1)
    vpmIntervalPNMap' = M.insert newInt2 newInt2 
                        $ M.insert newInt1 newInt1 
                        $ M.delete pnInterval intPNMap
    pnInterval     = intPNMap M.! (PortInterval portNum portNum)
    (newInt1, newInt2) = breakInterval pnInterval portNum



breakInterval :: PortInterval -> PortNum -> (PortInterval, PortInterval)
breakInterval (PortInterval l h) pn = (PortInterval l (pn-1), PortInterval (pn+1) h) 

intervalToPNs :: PortInterval -> [PortNum]
intervalToPNs (PortInterval l h) = [l .. h] 



a = undefined

