module Main where

import Test.Hspec
import Test.QuickCheck
import TicTacToe

main :: IO ()
main = hspec $ do
  describe "TTT - basic moves" $ do
    it "can start new game" $ do
      startNewGame `shouldBe` Board (10, 10) TypeEmpty []
    it "can make a correct move" $ do
      move startNewGame (Pos 0 0) `shouldBe` Board (10, 10) TypeInPlayOrFinished []
    it "rejects an incorrect move" $ do
      move (Board (10, 10) TypeFinished []) (Pos 0 0) `shouldBe` undefined

-- for example, test this property 
-- forall Board b. forall Position p. such that (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
