module Main where

main :: IO ()
main = putStrLn "ttt tests"

-- for example, test this property 
-- forall Board b. forall Position p. such that (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
