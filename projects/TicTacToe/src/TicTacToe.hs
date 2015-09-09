{-# LANGUAGE MultiParamTypeClasses #-}

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
module TicTacToe (
                   move
                 , whoWon
                 , playerAt
                 , takeBack
                 , startNewGame
                 , BoardEmptyOrInPlay(..)
                 , Position(Pos)
                 ) where

type Dimensins = (Int, Int)

defaultDimensions :: Dimensins
defaultDimensions = (10, 10)

type Coord = (Int, Int)

data Empty = Empty deriving (Show)
data InPlay = InPlay deriving (Show)
data Finished = Finished deriving (Show)

data Board a = BEmpty Dimensins
             | BInPlay Dimensins [Position]
             | BFinished Dimensins [Position] deriving (Show, Eq)

data BoardInPlayOrFinished = BIFInPlay (Board InPlay)
                           | BIFFinished (Board Finished) deriving (Show, Eq)

data BoardEmptyOrInPlay = BEIEmpty (Board Empty)
                        | BEIInPlay (Board InPlay) deriving (Show, Eq)

data Position = PosOccupied Coord Player
              | Pos Coord deriving (Show, Eq)

data Player = Player deriving (Show, Eq)

startNewGame :: Board Empty
startNewGame = BEmpty defaultDimensions

move :: BoardEmptyOrInPlay -> Position -> BoardInPlayOrFinished
move = undefined

whoWon :: Board Finished -> Player
whoWon = undefined

playerAt :: BoardInPlayOrFinished -> Position -> Maybe Player
playerAt = undefined

takeBack :: BoardInPlayOrFinished -> BoardEmptyOrInPlay
takeBack = undefined
