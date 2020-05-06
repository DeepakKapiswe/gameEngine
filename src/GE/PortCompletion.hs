module GE.PortCompletion where

import qualified Data.HashMap.Strict as M
import Data.List (delete)

import GE.Types
import GE.Common
import GE.PortMeta


-- | Close the port only if it is complete and not closed before otherwise
--   do nothing
closeCompletePort
  :: Coordinate       -- Coordinate of the Port which will be checked for closing
  -> PortMeta         -- PortMeta of the Port which will be checked for closing
  -> VisPortMeta
  -> VisPortMeta
closeCompletePort
  cord
  cordPM
  visPM@(VisPortMeta vpmMax vpmLastUpdPns vCPorts vOPorts vCPNMap vPNOPPMap) =
    case pIsClosed cordPM of
      False | checkIfComplete cordPM ->
        case M.lookup cord vCPNMap of 
          Nothing -> visPM 
          Just portNum -> 
             visPM { vpmLastUpdatedPNs = lastUpdPns'
                   , vpmCPorts         = vpmCPorts'
                   , vpmOPorts         = vpmOPorts'
                   , vpmCoordPNMap     = vpmCoordPNMap'
                   , vpmPNOPPMap       = vpmPNOPPMap'
                   }
            where
              vpmCPorts'     = M.insert cord (setIsClosed cordPM) vCPorts
              vpmOPorts'     = M.delete cord vOPorts
              lastUpdPns'    = delete portNum vpmLastUpdPns
              vpmCoordPNMap' = M.delete cord vCPNMap
              vpmPNOPPMap'   = M.delete portNum vPNOPPMap 
              
      _  -> visPM


  

