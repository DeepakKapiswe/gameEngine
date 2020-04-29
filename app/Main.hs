module Main where

import Data.Map

import GE.Types
-- import GE.Display
-- import GE.GamePlay
-- import GE.GameRun
-- import GE.CreateGame
-- import GE.MoveLogic

import Control.Monad

main :: IO ()
main = pure ()
  --  game (makeSampleGameWorld 10 10)



-- fun (u,r,d,l) ="{cD.isVis" ++code ++ " \t&& <img src={vis" ++ code ++"} \talt=\'vis"++code++ "\' />}"
--   where
--     code = u' ++ r' ++ d' ++ l'
--     u' = if u then "" else "U"
--     r' = if r then "" else "R"
--     d' = if d then "" else "D"
--     l' = if l then "" else "L"


-- fun (u,r,d,l) ="import vis" ++ code ++ " from \'./assets/vis"++ code ++".png\'"
--   where
--     code = u' ++ r' ++ d' ++ l'
--     u' = if u then "" else "U"
--     r' = if r then "" else "R"
--     d' = if d then "" else "D"
--     l' = if l then "" else "L"

aPM = [fun (a,b,c,d) | a <-t,b<-t,c<-t,d<-t ]

b = zipWith ($) aPM [17..]

t = [True, False]

p = forM_ k $ putStrLn

fun (u,r,d,l) x ="const cD" ++(show x) ++ " = { isVis"++code++" : true , }" 
  where
    code = u' ++ r' ++ d' ++ l'
    u' = if u then "" else "U"
    r' = if r then "" else "R"
    d' = if d then "" else "D"
    l' = if l then "" else "L"

k = ["{cellDetails:cD" ++show c ++"}"| c <- [1..32]]
