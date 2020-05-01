module GE.PortCompletion where

import qualified Data.HashMap.Strict as M
import Data.List (delete)

import GE.Types
import GE.Common

-- | This function should be called whenever a port is closed
portCloseHandler 
  :: Coordinate 
  -> VisPortMeta
  -> VisPortMeta
portCloseHandler 
  cord
  vpm@(VisPortMeta _ lastUpdPns _ _ cordPNMap pnOPPMap) =
    case M.lookup cord cordPNMap of 
      Nothing -> vpm
      Just portNum -> 
        vpm {
          vpmLastUpdatedPNs = lastUpdPns'
        , vpmCoordPNMap     = vpmCoordPNMap'
        , vpmPNOPPMap       = vpmPNOPPMap''
            }
        where
          lastUpdPns'    = delete portNum lastUpdPns
          vpmCoordPNMap' = M.delete cord cordPNMap
          vpmPNOPPMap'   = M.delete portNum pnOPPMap 
          vpmPNOPPMap''  = fmap (searchDeleteAdd cord) vpmPNOPPMap'

