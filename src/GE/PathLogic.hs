module GE.PathLogic where

import qualified Data.HashMap.Strict as M
import Data.List (delete)

import GE.Types
import GE.PortMeta
import GE.VisPortMeta
import GE.Robot
import GE.Common

updatePaths 
  :: Coordinate               -- The Coordinate of the last position of robot
  -> VisPortMeta              -- Visited Port Meta to update
  -> VisPortMeta              -- Updated Visited Port Meta with updated paths
updatePaths
  lastCord
  visPM@(VisPortMeta vpmMax vpmLastUpdPns vCPorts vOPorts vCPNMap vPNOPPMap) =
    case pIsFirstVisit lastPM of
      True -> visPM'           -- we can be sure this is the first time robot
                               -- visited this port. So just insert a new OPP
                               -- and update all previous without any check
                               -- and set the Fisrt Visit flag to False
        where
          visPM'     = insertPort lastCord pm' $ visPM {
              vpmMaxPortNum     = vpmMax'
            , vpmLastUpdatedPNs = vpmLastUpdPns'
            , vpmCoordPNMap     = vCPNMap'
            , vpmPNOPPMap       = vPNOPPMap'
          }
          vpmMax'    = succ vpmMax 
          vpmLastUpdPns' = vpmMax : vpmLastUpdPns
          vCPNMap'   = M.insert lastCord vpmMax vCPNMap
          vPNOPPMap' = M.insert vpmMax [lastCord] $ fmap ( lastCord : ) vPNOPPMap   
          pm'        = unsetFirstVisit lastPM 


      False -> visPM { vpmPNOPPMap = vPNOPPMap'}
        --   case pIsClosed lastPM of
        -- True  ->  visPM    -- Do Nothing as we can be sure whenever robot travels through
                           -- a Closed port then its destination must be an open port
                           -- and also we can be sure that the robot will must reach its
                           -- destination so there is no need to update the paths in between
                           -- as all the effect will be washed away when it will reach the
                           -- destination open port which was in history  
        -- False ->
        where
          vPNOPPMap' = fmap (searchDeleteAdd lastCord ) vPNOPPMap
  where
    lastPM = getCoordPM lastCord visPM                      
