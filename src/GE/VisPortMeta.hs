module GE.VisPortMeta where

import qualified Data.HashMap.Strict as M

import GE.Types
import GE.PortMeta
import GE.PortCompletion


initVisPorts :: VisPortMeta
initVisPorts = VisPortMeta 1 [] mempty mempty mempty mempty


insertPort
  :: Coordinate
  -> PortMeta
  -> VisPortMeta
  -> VisPortMeta
insertPort c pm vp = case checkIfComplete pm of
  False               -> vp { vpmOPorts = M.insert c pm $ vpmOPorts vp}
  True | pIsClosed pm -> vp { vpmCPorts = M.insert c pm $ vpmCPorts vp}
  True                -> vp''
    where
      vp'' = portCloseHandler c vp'
      vp' = vp { vpmCPorts = M.insert c pm' $ vpmCPorts vp}
      pm' = setIsClosed pm

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

-- ^^ for optimisation keep length data of both the maps and lookup
--    first on that map which is smaller

getCoordPM :: Coordinate -> VisPortMeta -> PortMeta
getCoordPM c visPM = case lookupPort c visPM of
  InOpen pm -> pm
  InClosed pm -> pm
  _           -> error "Oops! should not happen, in Port Completion" 