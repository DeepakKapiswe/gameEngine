module GE.Port where

import qualified Data.HashMap.Strict as M

import GE.Types
import GE.PortMeta


lookupPort
  :: Coordinate
  -> VisPortMeta
  -> PortLookupResult
lookupPort c vpm =
  case M.lookup c (vpmCPorts vpm) of
    Just cpm -> InClosed cpm
    _ -> case M.lookup c (vpmOPorts vpm) of
           Just opm -> InOpen opm
           _        -> PortNotFound

lookupPortRob
  :: Coordinate
  -> Robot
  -> PortLookupResult
lookupPortRob c rob =
  case M.lookup c (vpmCPorts vpm) of
    Just cpm -> InClosed cpm
    _ -> case M.lookup c (vpmOPorts vpm) of
           Just opm -> InOpen opm
           _ | elem c (rBlockedCoords rob) -> InBlocked
           _      -> PortNotFound
  where
    vpm = rVisPortMeta rob

-- ^^ for optimisation keep length data of both the maps and lookup
--    first on that map which is smaller



getCoordPM :: Coordinate -> VisPortMeta -> PortMeta
getCoordPM c visPM = case lookupPort c visPM of
  InOpen pm -> pm
  InClosed pm -> pm
  _           -> error "Oops! it should not happen, in Port Completion" 


