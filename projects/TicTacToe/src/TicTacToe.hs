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

class BoardTypeEmpty a where
  
class BoardTypeInPlay a where
  
class BoardTypeFinished a where
  
class BoardTypeEmptyOrInPlay a where
  
class BoardTypeInPlayOrFinished a where
  
data Board a = Board [Position] deriving (Show, Eq)

class Playable a where
  move :: (BoardTypeEmptyOrInPlay a, BoardTypeInPlayOrFinished b) => Board a -> Position -> Board b
  whoWon :: BoardTypeFinished a => Board a -> Player
  playerAt :: BoardTypeInPlayOrFinished a => Board a -> Position -> Maybe Player
  takeBack :: BoardTypeInPlay a =>  Board a -> Board a

data Position = Pos Int Int deriving (Show, Eq)

data Player = Player deriving (Show, Eq)
