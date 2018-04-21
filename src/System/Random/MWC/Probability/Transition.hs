{-# language OverloadedStrings #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
module System.Random.MWC.Probability.Transition (
  -- * Transition
    Transition
  , mkTransition
  , runTransition
  -- ** Helper functions
  , withSeverity
  -- -- * Re-exported from `logging-effect`
  -- , Handler
  -- , WithSeverity(..), Severity(..)
  -- -- , withFDHandler, defaultBatchingOptions
  ) where

import Control.Monad
import Control.Monad.Primitive

import qualified Control.Monad.State as S

import Control.Monad.Trans.Class (MonadTrans(..), lift)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT, execStateT, runStateT)
import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, withFDHandler, defaultBatchingOptions, logMessage)

import Data.Char

import System.Random.MWC.Probability



-- | A Markov transition kernel.
newtype Transition message s m a = Transition (
  Gen (PrimState m) -> StateT s (LoggingT message m) a
  ) deriving (Functor)

instance Show (Transition msg s m a) where
  show _ = "<Transition>"

-- | Construct a 'Transition' from sampling, state transformation and logging functions.
--
-- NB: The three function arguments are used in the order in which they appear here:
--
-- 1. a random sample @w :: t@ is produced, using the current state @x :: s@ as input
--
-- 2. output @z :: a@ and next state @x' :: s@ are computed using @w@ and @x@
--
-- 3. a logging message is constructed, using @z@ and @x'@ as arguments.
mkTransition :: Monad m =>
        (s -> Prob m t)     -- ^ Random generation
     -> (s -> t -> (a, s))  -- ^ (Output, Next state)
     -> (a -> s -> message) -- ^ Log message construction
     -> Transition message s m a
mkTransition fm fs flog = Transition $ \gen -> do
  s <- S.get
  w <- lift . lift $ sample (fm s) gen
  let (a, s') = fs s w
  lift $ logMessage $ flog a s' 
  S.put s'
  return a

-- | Run a 'Transition' for a number of steps, while logging each iteration.
runTransition :: Monad m =>
         Handler m message        -- ^ Logging handler
      -> Transition message s m a
      -> Int                      -- ^ Number of iterations 
      -> s                        -- ^ Initial state
      -> Gen (PrimState m)        -- ^ PRNG
      -> m ([a], s)               -- ^ (Outputs, Final state)
runTransition logf (Transition fm) n s0 g =
  runLoggingT (runStateT (replicateM n (fm g)) s0) logf




-- * Helpers
  
bracketsUpp :: Show a => a -> String
bracketsUpp p = unwords ["[", map toUpper (show p), "]"]

-- | Render a logging message along with an annotation of its severity.
withSeverity :: (t -> String) -> WithSeverity t -> String
withSeverity k (WithSeverity u a ) = unwords [bracketsUpp u, k a]


