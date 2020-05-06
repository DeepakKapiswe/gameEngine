module GE.VisPortMeta where

import qualified Data.HashMap.Strict as M

import GE.Types
import GE.PortMeta
import GE.PortCompletion 
import GE.Port


initVisPorts :: VisPortMeta
initVisPorts = VisPortMeta 1 [] mempty mempty mempty mempty


-- | Insert a new Port in VisPortMeta
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
      vp'' = closeCompletePort c pm vp'
      vp' = vp { vpmCPorts = M.insert c pm $ vpmCPorts vp}

-- | Update the PM with supplied function of the given coordinate in VisPortMeta 
updatePort :: Coordinate -> (PortMeta-> PortMeta) -> VisPortMeta -> VisPortMeta
updatePort cord f visPM = case M.lookup cord vCPorts of
  Just pm -> visPM {vpmCPorts = M.adjust f cord vCPorts}
  Nothing -> case M.lookup cord vOPorts of
    Just openPm -> closeCompletePort cord updPm $ visPM {vpmOPorts = M.adjust f cord vOPorts}
      where
        updPm = f openPm
    Nothing -> visPM
  where
    vCPorts = vpmCPorts visPM
    vOPorts = vpmOPorts visPM


-- | Try to get an OpenPortPath from the given conf of Robot
getOPP :: Robot -> Maybe [Coordinate]
getOPP rob | null vPNtoOPP = Nothing
           | otherwise = Just $ vPNtoOPP M.! lastUpdPortNum
  where
    vPNtoOPP = vpmPNOPPMap visPM
    lastUpdPortNum = head (vpmLastUpdatedPNs visPM)
    visPM    = rVisPortMeta rob

