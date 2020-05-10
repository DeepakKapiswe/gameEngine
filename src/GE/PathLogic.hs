{-# Language BangPatterns #-}
module GE.PathLogic where

import qualified Data.HashMap.Strict as M
import Data.List (delete)

import GE.Types
import GE.PortMeta
import GE.VisPortMeta
import GE.Robot
import GE.Common
import GE.Port
import GE.PortCompletion


updateRoboPath 
  :: Robot       -- Old Robot 
  -> Robot       -- New Robot
  -> [Direction] -- Last Robot Move Direction
  -> Robot       -- Robot With Paths Updated
updateRoboPath oldR newR moveDirs =
  newR { rVisPortMeta = updatedPaths }
  where
    updatedPaths = updatePaths lastPos visPM'  -- add last pos to all paths if required
    visPM' = trimPaths adjCords visPM         -- trim other paths if it can be visited via neighbour
    adjCords = 
      getAdjacentCordsFromDirs
        moveDirs
        oldR
        newR
    visPM   = rVisPortMeta newR
    lastPos = rPos oldR

getAdjacentCordsFromDirs
  :: [Direction]       -- List Of Last Directions
  -> Robot              -- Old Robot
  -> Robot              -- New Robot
  -> [Coordinate] 
getAdjacentCordsFromDirs
  moveDirs
  oldR
  newR = 
  fmap snd $
  filter 
    isValidAndVisitedBefore $
    dirToPmNeighbour lastPM <$> dirsToLook
  where
    dirsToLook = case moveDirs of
      []              -> [] 
      [x]             -> [] 
      [UP, UP]        -> [RIGHT, LEFT]
      [UP, RIGHT]     -> [UP, LEFT]
      [UP, DOWN]      -> [UP, RIGHT, LEFT]
      [UP, LEFT]      -> [UP, RIGHT]
      [RIGHT, UP]     -> [DOWN, RIGHT]
      [RIGHT, RIGHT]  -> [UP, DOWN]
      [RIGHT, DOWN]   -> [UP, RIGHT]
      [RIGHT, LEFT]   -> [RIGHT, DOWN, UP]
      [DOWN, UP]      -> [DOWN, LEFT, RIGHT]
      [DOWN, RIGHT]   -> [DOWN, LEFT]
      [DOWN, DOWN]    -> [RIGHT, LEFT]
      [DOWN, LEFT]    -> [DOWN, RIGHT]
      [LEFT, UP]      -> [LEFT, DOWN]
      [LEFT, RIGHT]   -> [LEFT, UP, DOWN]
      [LEFT, DOWN]    -> [LEFT,UP]
      [LEFT, LEFT]    -> [UP, DOWN]
      _               -> [] 

    visPM   = rVisPortMeta newR
    lastPos = rPos oldR
    lastPM  = getCoordPM lastPos visPM    

    dirToPmNeighbour pm UP    = (pUOpen pm, uNbr) 
    dirToPmNeighbour pm RIGHT = (pROpen pm, rNbr) 
    dirToPmNeighbour pm DOWN  = (pDOpen pm, dNbr) 
    dirToPmNeighbour pm LEFT  = (pLOpen pm, lNbr)

    nbrs = getNeighbours (rPos oldR)
    uNbr = nUp    nbrs  
    rNbr = nRight nbrs  
    dNbr = nDown  nbrs  
    lNbr = nLeft  nbrs

    isValidAndVisitedBefore :: (Bool, Coordinate) -> Bool
    isValidAndVisitedBefore (True, _)     = False
    isValidAndVisitedBefore (False, cord) = notElem cord $ rBlockedCoords newR


-- | Trim Open Port Paths if an adjacent port is present in OPP
--   otherwise leave it alone
trimPaths
  :: [Coordinate]     -- The Adjacent Coordinate list of last Position  
  -> VisPortMeta
  -> VisPortMeta
trimPaths
  adjacentCords
  visPM = 
    visPM { vpmPNOPPMap = vPNOPPMap' }
      where
        vPNOPPMap' = searchDeleteAll adjacentCords (vpmPNOPPMap visPM)

        searchDeleteAll [] pnOppMap = pnOppMap
        searchDeleteAll (x:xs) pnOppMap =
          let !newMap = searchDelete x <$> pnOppMap
          in searchDeleteAll xs newMap

updatePaths 
  :: Coordinate               -- The Coordinate of the last position of robot
  -> VisPortMeta              -- Visited Port Meta to update
  -> VisPortMeta              -- Updated Visited Port Meta with updated paths
updatePaths
  lastCord
  visPM@(VisPortMeta vpmMax vpmLastUpdPns vCPorts vOPorts vCPNMap vPNOPPMap) =
    updatePort lastCord unsetFirstVisit $
    closeCompletePort lastCord lastPM $
    case pIsFirstVisit lastPM of
      True | not $ pIsClosed lastPM -> visPM'          
                               -- we can be sure this is the first time robot
                               -- visited this port. So just insert a new OPP
                               -- and update all previous without any check
        where
          visPM' = updatePort lastCord unsetFirstVisit $ visPM {
              vpmMaxPortNum     = vpmMax'
            , vpmLastUpdatedPNs = vpmLastUpdPns'
            , vpmCoordPNMap     = vCPNMap'
            , vpmPNOPPMap       = vPNOPPMap'
          }
          vpmMax'    = succ vpmMax 
          vpmLastUpdPns' = vpmMax : vpmLastUpdPns
          vCPNMap'   = M.insert lastCord vpmMax vCPNMap
          vPNOPPMap' = M.insert vpmMax [lastCord] $ fmap ( lastCord : ) vPNOPPMap

      _ -> visPM { vpmPNOPPMap = vPNOPPMap'}
                                -- This means we have visited the port earlier
                                -- or the port was closed in firstvisit only
                                -- in both the cases we don't need to open a new port
                                -- but update all other paths as delete if this element
                                -- is present in the path till this element as those
                                -- are not necessary to travel as we found a cycle
        where
          vPNOPPMap' = fmap (searchDeleteAdd lastCord ) vPNOPPMap
  where
    lastPM = getCoordPM lastCord visPM                      


deleteCurrPortFromPaths
  :: Coordinate
  -> Robot
  -> Robot
deleteCurrPortFromPaths newPos rob =
  rob {rVisPortMeta = newVisPm}
  where
    newVisPm =  visPM { vpmPNOPPMap = vpmPNOPPMap'
                      }
    vpmPNOPPMap' = fmap (searchDelete newPos) (vpmPNOPPMap visPM)
    visPM = rVisPortMeta rob


