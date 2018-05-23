module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.Log

import System.Random.MWC.Probability
import System.Random.MWC.Probability.Transition


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "System.Random.MWC.Probability.Transition" $ do
    it "computes the final state of a sequence of deterministic transitions" $ do
      (_, z) <- runT01
      z `shouldBe` 5
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x


runT01 :: IO ([Double], Double)
runT01 = create >>= runTransition (putStrLn . withSeverity id) t01 5 0

t01 :: Monad m => Transition (WithSeverity String) Double m Double
t01 = mkTransition modelf statef logf where
  modelf _ = pure (1 :: Double)
  statef s t = (t, s + 1)
  logf s _ _ = WithSeverity Informational (show s)


