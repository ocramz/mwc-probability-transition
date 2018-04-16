{-# language OverloadedStrings, FlexibleContexts #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
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
import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, renderWithSeverity, withFDHandler, defaultBatchingOptions, logMessage, logDebug, logInfo, logNotice, logWarning, logError)

import System.Random.MWC.Probability






-- -- | Reference : we want a monad capable of these effects:
-- --
-- -- * Reading configuration
-- -- * Logging
-- -- * Transforming state
-- -- * Sampling random variables
-- -- adsf :: (MonadLog (WithSeverity )S.MonadState b m, R.MonadReader t m) =>
-- --         (t -> Prob m a)
-- --      -> Gen (PrimState m)
-- --      -> m b
-- adsf fsample gen = do
--   x <- S.get
--   c <- R.ask
--   w <- sample (fsample c x) gen
--   logInfo w
--   S.put x
--   return x

newtype T msg s m a =
  T { unT :: LoggingT msg (StateT s (Prob m)) a }
      deriving (Functor, Applicative, Monad)

instance MonadTrans (T msg s) where
  lift = T . lift . lift . lift

instance MonadLog msg m => MonadLog msg (T msg s m)

--

newtype T2 msg s m a =
  T2 { unT2 :: StateT s (LoggingT msg (Prob m)) a }
       deriving (Functor, Applicative, Monad, S.MonadState s)

instance MonadTrans (T2 msg s) where
  lift = T2 . lift . lift . lift

instance MonadLog msg m => MonadLog msg (T2 msg s m)

t2 gen = T2 $ do
  x <- S.get
  w <- lift $ sample (normal 1 2) gen
  S.put x

t3 :: (S.MonadState s m, MonadLog (WithSeverity String) m) => m ()
t3 = do
  x <- S.get
  logInfo ("moo" :: String)
  S.put x

--

newtype T4 s m a = T4 { unT4 :: StateT s (Prob m) a }

class MonadProb m a where
  mpSample :: Prob m a -> Gen (PrimState m) -> m a
  mpSample = sample  

t4 pf gen = do
  x <- S.get
  w <- mpSample pf gen
  logInfo ("moo" :: String)
  S.put x
  return x


--

newtype T5 s m a = T5 { unT5 :: Gen (PrimState m) -> StateT s m a }

t5 :: PrimMonad m => T5 s m ()
t5 = T5 t5'

t5' :: PrimMonad m => Gen (PrimState m) -> StateT s m ()
t5' gen = do
  x <- get
  w <- sample (normal 1 2 ) gen
  put x


--

-- t6 :: Monad m => (b -> Prob m a) -> Gen (PrimState m) -> StateT b m a
t6 :: MonadLog (WithSeverity msg) m =>
      (s -> Prob m a)
   -> (a -> s -> (Severity, msg))
   -> Gen (PrimState m)
   -> StateT s m a
t6 mf logf gen = StateT $ \s -> do
  w <- sample (mf s) gen
  let (lsev, logEntry) = logf w s
  logMessage (WithSeverity lsev logEntry)
  return (w, s)


-- newtype T6 s m a = T6 { unT6 :: Gen (PrimState m) -> StateT s m a } deriving (Functor, Monad)


-- t6' gen = do
--   x <- get
--   w <- sample (normal 1 2 ) gen
--   logInfo ("moo" :: String)  
--   put x
  

-- 



-- newtype T msg m a = T {
--   unT :: LoggingT msg (Prob m) a }
--      deriving (Functor, Applicative, Monad)

-- mkT h = T $ LoggingT $ ReaderT h

-- -- asdf :: ((msg -> m ()) -> m a) -> LoggingT msg m a
-- -- asdf h = LoggingT $ ReaderT $ do
  




-- -- LoggingT / StateT / Prob

-- newtype Trans msg s m a = Trans {
--   unTrans :: LoggingT msg (StateT s (Prob m)) a
--                                 } deriving (Functor, Applicative, Monad, S.MonadState s)


-- -- liftProb :: MonadIO m => IO (a, s) -> Trans msg s m a
-- -- liftProb io = Trans $ LoggingT $ ReaderT $ \ _ -> StateT $ pure $ liftIO io

-- -- -- instance MonadIO m => MonadIO (Trans msg s m) where
-- -- --   liftIO = liftProb 


-- instance MonadTrans (Trans msg s) where
--   lift = Trans . lift . lift . lift

-- instance PrimMonad m => PrimMonad (Trans msg s m) where
--   type PrimState (Trans msg s m) = PrimState m
--   primitive = lift . primitive
--   {-# INLINE primitive #-}



-- -- test :: (PrimMonad m, MonadLog (WithSeverity String) m) =>
-- --     Gen (PrimState m) -> Trans msg s m ()
-- -- test gen = Trans $ do
-- --   x <- S.get
-- --   logInfo ("moo" :: String)
-- --   w <- sample (normal 1 2) gen
-- --   S.put x




-- ReaderT / LoggingT / StateT / Prob


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
  
  
