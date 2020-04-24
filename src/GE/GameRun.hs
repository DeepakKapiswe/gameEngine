module GE.GameRun where

import System.IO (stdin,hReady,hSetEcho)


import GE.Types
import GE.GamePlay
import GE.Display


runGame :: GameWorld -> IO ()
runGame g = do
  putStr "\ESC[2J"              -- Clear Screen
  putStr $ gameWorldToStr g     -- Print Game
--   print $ gwRobot g
  key <- getKey                 -- wait for input
  case keyToCommand key of
    Nothing               -> putStr "\BEL" >> runGame g
    Just (SetDirection d) -> runGame $ setRoboDir d g
    Just Move             -> 
        case moveReq g of
          Left _   -> do
              putStrLn "\BEL"
              runGame g
          Right g' -> runGame g'


game :: GameWorld -> IO ()
game g = do
  hSetEcho stdin False
  runGame g


getKey = reverse <$> getKey' ""
getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

keyToCommand :: String -> Maybe Command
keyToCommand input = case input of
  "a"      -> Just Move
  "\ESC[A" -> Just $ SetDirection UP  
  "\ESC[C" -> Just $ SetDirection RIGHT 
  "\ESC[B" -> Just $ SetDirection DOWN  
  "\ESC[D" -> Just $ SetDirection LEFT
  _        -> Nothing