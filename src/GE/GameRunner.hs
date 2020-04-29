module GE.GameRunner where


import GE.Types


gameServer :: GameWorld -> Coordinate -> MoveRes
gameServer gw c = case any (elem c) (gwGrid gw) of
    True -> Ok c
    _ -> Blocked c

