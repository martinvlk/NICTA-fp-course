{-|
Module      : TicTacToe
Description : A type-safe API for the game of Tic Tac Toe.
Copyright   : (c) Martin Vlk, 2015
License     : GPL-3
Maintainer  : martin@vlkk.cz
Stability   : experimental
Portability : POSIX

The main library for the game.
-}
module TicTacToe ( move
                 , whoWon
                 , playerAt
                 , takeBack) where

-- |A type that holds the game data.
data Board = BEmpty
           | BInPlay
           | BFinished deriving (Show, Eq)

data Position = Pos Int Int deriving (Show, Eq)

data Player = Player deriving (Show, Eq)

move :: Board -> Position -> Board
move = undefined

whoWon :: Board -> Player
whoWon = undefined

playerAt :: Board -> Position -> Maybe Player
playerAt = undefined

takeBack :: Board -> Board
takeBack = undefined

