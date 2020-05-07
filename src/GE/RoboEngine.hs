module GE.RoboEngine where

import GE.Types
import GE.VisPortMeta
import GE.PortMeta
import GE.Port
import GE.Robot

getNextCommand 
  :: RoboEngineData
  -> MoveRes
  -> Robot
  -> (RoboEngineData, Command)     -- New RoboEngine Data and Command to execute

getNextCommand 
  red           -- Robo Engine Data
  mr            -- Move Response of last Request
  rob = 
    case redMMode red of
      TrappedMode | not $ null (redNextCommands red)  -> (red', cmd)   -- valid action to take
        where
          red' = red {redNextCommands = rest}
          cmd:rest = redNextCommands red
      TrappedMode -> (red', UpdateEngineData) -- sucessfully reached destination open port
        where
          red' = red {redMMode = NormalMode}
      NormalMode ->
        case mr of
          Ok _ | not $ null (redNextCommands red) -> (red', cmd)   -- valid action to take
            where
              red' = red {redNextCommands = rest}
              cmd:rest = redNextCommands red
          DirectionSet _ | not $ null (redNextCommands red)  -> (red', cmd)   -- valid action to take
            where
              red' = red {redNextCommands = rest}
              cmd:rest = redNextCommands red

          Ok _ ->     -- Ok with CmdList Empty so have to set new Direction and extreme points
           case getNewMoveDirection (rDir rob) (rPortMeta rob) of
             Nothing -> case checkAdjacentIfOpen rob of
               Just d -> (red', cmd)
                 where
                   cmd:rest = if d /= (rDir rob) then SetDirection d : [Move] else [Move]
                   red' = red { redNextCommands = rest }
               Nothing -> case getOPP rob of  -- means the port is closed
                 Nothing -> (red, Done)              -- all visited
                 Just openPortPath -> (red', cmd)      
                   where
                     red' = red { redMMode = TrappedMode
                                , redNextCommands = rest 
                                , redDestination = Just openPortPath }
                     cmd:rest = genPathCommands (rPos rob) (rDir rob) openPortPath

             Just d | inExtremePos (rPos rob) (rDir rob) red -> (red'', cmd)    -- change direction, set new extreme and execute
               where
                   cmd:rest = SetDirection d : moveCmds 
                   moveCmds = getLinearMoveCommands (rPos rob) newExtreme d
                   newExtreme = case d of
                     UP    ->  getNewExtreme UP    (rPos rob) $ redUExtreme red
                     RIGHT ->  getNewExtreme RIGHT (rPos rob) $ redRExtreme red
                     DOWN  ->  getNewExtreme DOWN  (rPos rob) $ redDExtreme red
                     LEFT  ->  getNewExtreme LEFT  (rPos rob) $ redLExtreme red
                   
                   red'' = case d of
                     UP    ->  red { redUExtreme = newExtreme, redNextCommands = rest  }
                     RIGHT ->  red { redRExtreme = newExtreme, redNextCommands = rest  }
                     DOWN  ->  red { redDExtreme = newExtreme, redNextCommands = rest  }
                     LEFT  ->  red { redLExtreme = newExtreme, redNextCommands = rest  }
             Just d | checkMoveDirection (rDir rob) (rPortMeta rob) -> (red', Move)
               where
                 red' = red { redNextCommands = [] }
                 
             Just d | crossedExtremePos (getNextPosInDir (rPos rob) d) d red  -> (red'', cmd)
               where
                 cmd:rest = SetDirection d : [Move]
                 newExtreme = case d of
                   UP    ->  getNewExtreme UP    (rPos rob) $ redUExtreme red
                   RIGHT ->  getNewExtreme RIGHT (rPos rob) $ redRExtreme red
                   DOWN  ->  getNewExtreme DOWN  (rPos rob) $ redDExtreme red
                   LEFT  ->  getNewExtreme LEFT  (rPos rob) $ redLExtreme red
                   
                 red'' = case d of
                   UP    ->  red { redUExtreme = newExtreme, redNextCommands = rest  }
                   RIGHT ->  red { redRExtreme = newExtreme, redNextCommands = rest  }
                   DOWN  ->  red { redDExtreme = newExtreme, redNextCommands = rest  }
                   LEFT  ->  red { redLExtreme = newExtreme, redNextCommands = rest  }
             
             Just d -> (red', cmd)    -- change direction, move once 
               where
                   cmd:rest = if d /= (rDir rob) then SetDirection d : [Move] else [Move]
                   red' = red { redNextCommands = rest }
          _ ->     
           case getNewMoveDirection (rDir rob) (rPortMeta rob) of
             Nothing -> case checkAdjacentIfOpen rob of
               Just d -> (red', cmd)
                 where
                   cmd:rest = if d /= (rDir rob) then SetDirection d : [Move] else [Move]
                   red' = red { redNextCommands = rest } 
               Nothing -> case getOPP rob of         -- means the port is closed
                 Nothing -> (red, Done)              -- all visited
                 Just openPortPath -> (red', cmd)      
                   where
                     red' = red { redMMode = TrappedMode
                                , redNextCommands = rest
                                , redDestination = Just openPortPath }
                     cmd:rest = genPathCommands (rPos rob) (rDir rob) openPortPath

             Just d | crossedExtremePos (getNextPosInDir (rPos rob) d) d red  -> (red'', cmd)
               where
                 cmd:rest = SetDirection d : [Move]
                 newExtreme = case d of
                   UP    ->  getNewExtreme UP    (rPos rob) $ redUExtreme red
                   RIGHT ->  getNewExtreme RIGHT (rPos rob) $ redRExtreme red
                   DOWN  ->  getNewExtreme DOWN  (rPos rob) $ redDExtreme red
                   LEFT  ->  getNewExtreme LEFT  (rPos rob) $ redLExtreme red
                   
                 red'' = case d of
                   UP    ->  red { redUExtreme = newExtreme, redNextCommands = rest  }
                   RIGHT ->  red { redRExtreme = newExtreme, redNextCommands = rest  }
                   DOWN  ->  red { redDExtreme = newExtreme, redNextCommands = rest  }
                   LEFT  ->  red { redLExtreme = newExtreme, redNextCommands = rest  } 
             Just d -> (red', cmd)    -- change direction, move once 
               where
                   cmd:rest = if d /= (rDir rob) then SetDirection d : [Move] else [Move]
                   red' = red { redNextCommands = rest }
              
      

initRoboEngineData :: Coordinate -> RoboEngineData
initRoboEngineData initPos = 
  RoboEngineData
    initPos initPos initPos initPos NormalMode [] Nothing

-- | This function will return a new direction looking clockwise if open
--   otherwise if Trapped i.e all the current port is complete it will return
--   Nothing
getNewMoveDirection :: Direction -> PortMeta -> Maybe Direction
getNewMoveDirection dir pm@(PortMeta u r d l _ _) 
  | checkIfComplete pm = Nothing 
  | otherwise = Just . getClockWiseDir $ 
    case dir of
      UP    -> [r',d',l',u']
      RIGHT -> [d',l',u',r']
      DOWN  -> [l',u',r',d']
      LEFT  -> [u',r',d',l']
  where
    u' = (UP, u)
    r' = (RIGHT,r)
    d' = (DOWN, d)
    l' = (LEFT, l)
    getClockWiseDir = fst . head . dropWhile (\(_,p) -> p /= True)

checkMoveDirection :: Direction -> PortMeta -> Bool
checkMoveDirection dir pm@(PortMeta u r d l _ _) 
  | checkIfComplete pm = False 
  | otherwise = 
    case dir of
      UP    -> u
      RIGHT -> r
      DOWN  -> d
      LEFT  -> l


genPathCommands
  :: Coordinate     -- current Source Coordinate of Robot 
  -> Direction      -- current facing direction of Robot
  -> [Coordinate]   -- Path to follow to reach destination port
  -> [Command]      -- List of command generated
genPathCommands currPos dir pathCords =
  go currPos dir [] (if head pathCords == currPos then (drop 1 pathCords) else pathCords)
  where
    go curr dir cmds []     = mconcat $ reverse cmds
    go curr dir acc  (c:cs) =
      let newCmds = getOneStepCommand curr c dir
          newDir  = 
            case newCmds of
              (SetDirection d' : mvCmds) | d' /= dir -> d'
              _                                      -> dir
      in  go c newDir (newCmds : acc) cs


getOneStepCommand 
  :: Coordinate     -- Current Position
  -> Coordinate     -- Destination Neighbour Coordinate
  -> Direction      -- Current Robo Direction
  -> [Command]      -- Returned Command to reach Destination
getOneStepCommand
  currPos destPos currDir =
    case getDirection currPos destPos of
      d | d /= currDir -> [SetDirection d, Move]
      _                -> [Move]
      

-- | A helper function to calculate the direction required to
--   go to destination port from source port

getDirection
  :: Coordinate     -- Source Coordinate
  -> Coordinate     -- Destination Coordinate
  -> Direction      -- Direction 
getDirection
  src@(Coordinate sx sy) des@(Coordinate dx dy)
    | sx == dx && sy <  dy = UP
    | sx <  dx && sy == dy = RIGHT
    | sx == dx && sy >  dy = DOWN
    | sx >  dx && sy == dy = LEFT


checkAdjacentIfOpen :: Robot -> Maybe Direction
checkAdjacentIfOpen rob = case lookupPortRob uNbr rob of
  PortNotFound -> pure UP 
  InOpen _     -> pure UP 
  _ -> case lookupPortRob rNbr rob of
    PortNotFound -> pure RIGHT 
    InOpen _     -> pure RIGHT 
    _ -> case lookupPortRob dNbr rob of
      PortNotFound -> pure DOWN
      InOpen _     -> pure DOWN
      _ -> case lookupPortRob lNbr rob of
        PortNotFound -> pure LEFT
        InOpen _     -> pure LEFT
        _ -> Nothing
  where
    nbrs = getNeighbours $ rPos rob
    uNbr = nUp    nbrs  
    rNbr = nRight nbrs  
    dNbr = nDown  nbrs  
    lNbr = nLeft  nbrs

getNewExtreme 
  :: Direction   -- The direction to look new extreme for
  -> Coordinate  -- Current Position of Robot
  -> Coordinate  -- Current Extreme Port in Given Direction
  -> Coordinate
getNewExtreme d (Coordinate x y) (Coordinate ex ey)= case d of
  UP    -> Coordinate x     (ey+1) 
  RIGHT -> Coordinate (ex+1) y
  DOWN  -> Coordinate x     (ey-1)
  LEFT  -> Coordinate (ex-1) y


-- | Gives list of Move commands required to reach the destination port
--   from source port in the same axis
getLinearMoveCommands
  :: Coordinate          -- Current Position of Robot Source Port
  -> Coordinate          -- Destination Port 
  -> Direction           -- The Direction in which to move
  -> [Command]
getLinearMoveCommands
  src@(Coordinate sx sy) des@(Coordinate dx dy) dir =
  let numStep = case dir of
                  UP    -> abs (dy - sy) 
                  RIGHT -> abs (dx - sx)
                  DOWN  -> abs (sy - dy)
                  LEFT  -> abs (sx - dx)
  in replicate numStep Move

inExtremePos :: Coordinate -> Direction -> RoboEngineData -> Bool
inExtremePos currPos@(Coordinate cx cy) dir red =
  case dir of
     UP    -> cy >= uy
     RIGHT -> cx >= rx
     DOWN  -> cy <= dy
     LEFT  -> cx <= lx
  where
    Coordinate ux uy = redUExtreme red
    Coordinate rx ry = redRExtreme red
    Coordinate dx dy = redDExtreme red
    Coordinate lx ly = redLExtreme red

crossedExtremePos :: Coordinate -> Direction -> RoboEngineData -> Bool
crossedExtremePos currPos@(Coordinate cx cy) dir red =
  case dir of
     UP    -> cy > uy
     RIGHT -> cx > rx
     DOWN  -> cy < dy
     LEFT  -> cx < lx
  where
    Coordinate ux uy = redUExtreme red
    Coordinate rx ry = redRExtreme red
    Coordinate dx dy = redDExtreme red
    Coordinate lx ly = redLExtreme red

-- | check whether robot can travel in the given path or its invalid path
checkConsistency :: [Coordinate] -> Bool
checkConsistency [] = True
checkConsistency [x] = True
checkConsistency (x@(Coordinate sx sy) : y@(Coordinate dx dy) : xs) 
  | sx == dx && succ sy ==  dy = checkConsistency (y:xs)
  | succ sx ==  dx && sy == dy = checkConsistency (y:xs) 
  | sx == dx && sy ==  succ dy = checkConsistency (y:xs) 
  | sx == succ dx && sy == dy = checkConsistency (y:xs)
  | otherwise = error $ "Inconsistant path: " <> (show x)<> "-->>" <> (show y)  
 
