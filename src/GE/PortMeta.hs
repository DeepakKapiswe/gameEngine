module GE.PortMeta where


import GE.Types

setUOpen :: PortMeta -> PortMeta
setUOpen p = p {pUOpen = False}

setROpen :: PortMeta -> PortMeta
setROpen p = p {pROpen = False}

setDOpen :: PortMeta -> PortMeta
setDOpen p = p {pDOpen = False}

setLOpen :: PortMeta -> PortMeta
setLOpen p = p {pLOpen = False}


setPM :: Direction -> PortMeta -> PortMeta
setPM d = case d of
  UP    -> setUOpen 
  RIGHT -> setROpen 
  DOWN  -> setDOpen 
  LEFT  -> setLOpen

initPortMeta :: PortMeta
initPortMeta = PortMeta True True True True True False 

closePortIfPossible :: PortMeta -> PortMeta
closePortIfPossible pm@ (PortMeta u r d l _ _)
  | not $ and [u,r,d,l] = pm { pIsClosed = True }
  | otherwise = pm