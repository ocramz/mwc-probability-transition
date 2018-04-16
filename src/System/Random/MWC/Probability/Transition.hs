module System.Random.MWC.Probability.Transition where

import Control.Monad

import Control.Monad.Primitive
-- import qualified Control.Monad.State as S

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, evalStateT)
import Control.Monad.Trans.Reader
import Control.Monad.Log

import System.Random.MWC.Probability



-- | A type for Markov random walks
newtype Transition m a =
  Transition { runTransition :: Gen (PrimState m) -> StateT a m a }

  
sampleSDE' msf f gen = do
  x <- lift get
  c <- ask
  w <- lift . lift $ sample (msf c) gen
  let z = f x w
  lift $ put z
  return z
