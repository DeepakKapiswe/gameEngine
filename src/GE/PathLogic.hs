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
  -> Robot       -- Robot With Paths Updated
updateRoboPath oldR newR =
  newR { rVisPortMeta = updatePaths lastPos visPM }
  where
    visPM = rVisPortMeta newR
    lastPos = rPos oldR

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


