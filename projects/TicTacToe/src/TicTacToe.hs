{-|
Module      : TicTacToe
Description : A type-safe API for the game of Tic Tac Toe.
Copyright   : (c) Martin Vlk, 2015
License     : GPL-3
Maintainer  : martin@vlkk.cz
Stability   : experimental
Portability : POSIX

The main library for the game.

The concept for the types is such that there is a basic Board type, designed to hold one specific board state (empty, inplay or finished).

In places where we need to accept more than one specific Board type, we use wrapper types that allow the desired combination of basic Board types.

We only export minimum necessary data constructors so that it is not possible for the API user to manipulate the game state, except using the API functions. They can examine the game state freely.
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

type Coord = (Int, Int)

data Empty = Empty deriving (Show)
data InPlay = InPlay deriving (Show)
data Finished = Finished deriving (Show)

data Board a = BEmpty Dimensins
             | BInPlay Dimensins GameState
             | BFinished Dimensins GameState deriving (Show, Eq)

data BoardInPlayOrFinished = BIFInPlay (Board InPlay)
                           | BIFFinished (Board Finished) deriving (Show, Eq)

data BoardEmptyOrInPlay = BEIEmpty (Board Empty)
                        | BEIInPlay (Board InPlay) deriving (Show, Eq)

data BoardAny = AnyEmpty (Board Empty)
              | AnyInPlay (Board InPlay)
              | AnyFinished (Board Finished) deriving (Show, Eq)

data Position = PosOccupied Coord Player
              | Pos Coord deriving (Show, Eq)

newtype GameState = GameState [Position] deriving (Show, Eq)

data Player = Noughts
            | Crosses deriving (Show, Eq)

data InvalidMoveError = InvalidMoveError InvalidMoveDetails deriving (Show)
data InvalidMoveDetails = InconsistentBoard
                        | OutOfBoardCoord Coord
                        | PositioOccupied Position
                        | InconsistentPosition Position deriving (Show)

startNewGame :: Board Empty
startNewGame = BEmpty defaultDimensions

defaultDimensions :: Dimensins
defaultDimensions = (10, 10)

move :: BoardEmptyOrInPlay -> Position ->
        Either InvalidMoveError BoardInPlayOrFinished
move (BEIEmpty (BEmpty dim)) pos = makeMove (GameState []) $ validPos dim pos
move (BEIInPlay (BInPlay dim gs)) pos = makeMove gs $ validPos dim pos
move _ _ = Left $ InvalidMoveError InconsistentBoard

validPos :: Dimensins -> Position -> Either InvalidMoveError Position
validPos (w, h) pos@(Pos c@(x, y))
  | x > 0 && x <= w &&
    y > 0 && y <= h = Right pos
  | otherwise       = Left $ InvalidMoveError $ OutOfBoardCoord c
validPos _ p@(PosOccupied{}) = Left $ InvalidMoveError $ InconsistentPosition p

makeMove :: GameState -> Either InvalidMoveError Position ->
            Either InvalidMoveError BoardInPlayOrFinished
makeMove _ (Left e) = Left e
makeMove (GameState positions) (Right pos)
  | pos `elem` positions = Left $ InvalidMoveError $ PositioOccupied pos
  | otherwise = undefined

whoWon :: Board Finished -> Maybe Player
whoWon = undefined

playerAt :: BoardAny -> Position -> Maybe Player
playerAt = undefined

takeBack :: BoardInPlayOrFinished -> BoardEmptyOrInPlay
takeBack = undefined
