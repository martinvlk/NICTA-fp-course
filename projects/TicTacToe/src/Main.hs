module Main where

import TicTacToe

main :: IO ()
main = putStrLn "ttt"

tryPlay = let bempty = startNewGame
              b1 = move (BEIEmpty bempty) $ Pos (1, 1)
          in undefined

