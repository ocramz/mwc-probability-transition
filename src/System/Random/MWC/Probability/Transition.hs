{-# language OverloadedStrings, FlexibleContexts #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
module System.Random.MWC.Probability.Transition where

import Control.Monad
import Control.Monad.Primitive

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S

import Control.Monad.Trans.Class (MonadTrans(..), lift)
import Control.Monad.Trans.State.Strict (StateT(..), get, put, evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Primitive
import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, renderWithSeverity, withFDHandler, defaultBatchingOptions, logDebug, logInfo, logNotice, logWarning, logError)

import System.Random.MWC.Probability





newtype Trans msg s m a = Trans {
  unTrans :: LoggingT msg (StateT s (Prob m)) a
                                } deriving (Functor, Applicative, Monad)

instance Monad m => S.MonadState s (Trans msg s m)

liftProb :: MonadIO m => IO (a, s) -> Trans msg s m a
liftProb io = Trans $ LoggingT $ ReaderT $ \ _ -> StateT $ pure $ liftIO io

instance MonadIO m => MonadIO (Trans msg s m) where
  liftIO = liftProb 


instance MonadTrans (Trans msg s) where
  lift = Trans . lift . lift . lift

test gen = Trans $ do
  x <- S.get
  logInfo ("moo" :: String)
  w <- sample (normal 1 2) gen
  S.put x

-- newtype Transition c msg s m a = Transition {
--   runTrans :: ReaderT c (LoggingT msg (StateT s (Prob m))) a}
--    deriving (Functor, Applicative, Monad, S.MonadState s, R.MonadReader c, MonadLog msg)

-- --  MonadIO m => MonadIO (ReaderT r m)
-- --  MonadIO m => MonadIO (LoggingT message m)
-- --  MonadIO m => MonadIO (StateT s m)
-- --  MonadIO m => MonadIO (Prob m)


-- instance MonadTrans (Transition c msg s) where
--   lift = Transition . lift . lift . lift . lift

-- -- instance MonadLog msg m => MonadLog msg (Transition c msg s m) where
 
-- execTrans :: Monad m =>
--              r
--           -> (msg -> m ())
--           -> s
--           -> Gen (PrimState m)
--           -> Transition r msg s m a
--           -> m (a, s)
-- execTrans c h x0 gen (Transition m) =
--   sample (runStateT (runLoggingT (runReaderT m c) hlift) x0) gen
--   where
--     hlift msg = lift (lift $ h msg)

-- test gen = do
--   x <- S.get
--   w <- sample (normal 1 2) gen
--   logInfo ("moo" :: String)
--   S.put x





-- test gen = do
--   x <- get
--   c <- lift . lift . lift $ ask 
--   -- w <- lift $ sample (normal 1 2) gen
--   put x


-- -- | A type for Markov random walks
-- newtype Transition m a =
--   Transition { runTransition :: Gen (PrimState m) -> StateT a m a } 

  
-- sampleSDE' msf f gen = do
--   x <- lift get
--   c <- ask
--   w <- lift . lift $ sample (msf c) gen
--   let z = f x w
--   lift $ put z
--   return z



-- asdf msf f gen = do
--   x <- get
--   y <- lift $ sample msf gen
--   logInfo ("moo" :: String)
--   let z = f x y
--   put z
--   return z
  
  
