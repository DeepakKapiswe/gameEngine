module GE.VisPorts where

import qualified Data.Map as M

import GE.Types


initVisPorts :: VisPortMeta
initVisPorts = VisPortMeta 2 1 mempty mempty mempty mempty


-- insertOPort
--   :: Coordinate
--   -> PortMeta
--   -> VisPorts
--   -> VisPorts
-- insertOPort c pm vp = vp { vOPorts = M.insert c pm }

