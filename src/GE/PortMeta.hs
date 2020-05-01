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

setIsClosed :: PortMeta -> PortMeta
setIsClosed p = p {pIsClosed = True}


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
  | or [u,r,d,l] = pm
  | otherwise = pm { pIsClosed = True }

unsetFirstVisit :: PortMeta -> PortMeta
unsetFirstVisit pm = pm {pIsFirstVisit = False}


checkIfComplete :: PortMeta -> Bool
checkIfComplete pm@(PortMeta u r d l _ _) = not $ or [u,r,d,l] 