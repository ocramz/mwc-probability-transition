{-# language OverloadedStrings #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
module System.Random.MWC.Probability.Transition (
  -- * Transition
    Transition
  , mkTransition
  , runTransition
  -- ** Helper functions
  , withSeverity
  ) where

import Control.Monad
import Control.Monad.Primitive

import qualified Control.Monad.State as S

import Control.Monad.Trans.Class (MonadTrans(..), lift)
import Control.Monad.Trans.State.Strict (StateT(..), get, put, evalStateT, execStateT, runStateT)
import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, renderWithSeverity, withFDHandler, defaultBatchingOptions, logMessage, logDebug, logInfo, logNotice, logWarning, logError)

import Data.Char

import System.Random.MWC.Probability



-- | A Markov transition kernel.
newtype Transition message s m a = Transition (
  Gen (PrimState m) -> StateT s (LoggingT message m) a
  ) deriving (Functor)

-- | Construct a 'Transition' from sampling, state transformation and logging functions.
mkTransition :: Monad m =>
        (s -> Prob m t)     -- ^ Random generation
     -> (s -> t -> (a, s))  -- ^ State transformation
     -> (a -> s -> message) -- ^ Log message generation
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
      -> m [(a, s)]
runTransition logf (Transition fm) n s0 g =
  runLoggingT (replicateM n (runStateT (fm g) s0)) logf



  
bracketsUpp :: Show a => a -> String
bracketsUpp p = unwords ["[", map toUpper (show p), "]"]

-- | Render a logging message along with an annotation of its severity.
withSeverity :: (t -> String) -> WithSeverity t -> String
withSeverity k (WithSeverity u a ) = unwords [bracketsUpp u, k a]


