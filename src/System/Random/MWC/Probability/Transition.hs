module System.Random.MWC.Probability.Transition where

import Control.Monad

import Control.Monad.Primitive
-- import qualified Control.Monad.State as S

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(..), get, put, evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Log (MonadLog(..), WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, renderWithSeverity, withFDHandler, defaultBatchingOptions, logDebug, logInfo, logNotice, logWarning, logError)

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
