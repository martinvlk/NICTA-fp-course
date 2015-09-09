module Main where

import TicTacToe

main :: IO ()
main = putStrLn "ttt"

tryPlay = let bempty = startNewGame
          in move (BEIEmpty bempty) $ Pos (1, 1)

