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



-- | Reference : we want a monad capable of these effects:
--
-- * Reading configuration
-- * Logging
-- * Transforming state
-- * Sampling random variables



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
  T { unT :: LoggingT (WithSeverity msg) (StateT s (Prob m)) a }
      deriving (Functor, Applicative, Monad)

instance MonadTrans (T msg s) where
  lift = T . lift . lift . lift

instance MonadLog msg m => MonadLog msg (T msg s m)

runT :: Monad m =>
        T msg s m a
     -> (WithSeverity msg -> m ())
     -> s
     -> Prob m (a, s)
runT (T m) lh x0 = runStateT (runLoggingT m lhLift) x0
  where
    lhLift msg = lift $ lift (lh msg)


-- t :: (Monad m, Num s, PrimMonad (LoggingT (WithSeverity String) (StateT s (Prob m)))) => Gen (PrimState (LoggingT (WithSeverity String) (StateT s (Prob m)))) -> T String s m Double
-- t gen = T $ do
--   logInfo ("moo" :: String)
--   S.modify (+ 1)
--   sample (normal 1 2) gen


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

-- | mtl + MonadLog

t3 :: (S.MonadState s m, MonadLog (WithSeverity String) m) => m ()
t3 = do
  x <- S.get
  logInfo ("moo" :: String)
  S.put x

-- | StateT / Prob

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

t6 :: MonadLog (WithSeverity msg) m =>
      (s -> Prob m a)         -- ^ Sampling function (using current state)
   -> (a -> s -> s)           -- ^ State update
   -> (s -> WithSeverity msg) -- ^ Log generation from updated state
   -> Gen (PrimState m)       -- ^ PRNG
   -> StateT s m a
t6 mf stf logf gen = StateT $ \s -> do
  w <- sample (mf s) gen
  let s' = stf w s
      le@(WithSeverity _ _) = logf s'
  logMessage le
  return (w, s')


-- newtype T6 s m a = T6 { unT6 :: Gen (PrimState m) -> StateT s m a } deriving (Functor, Monad)


-- t6' gen = do
--   x <- get
--   w <- sample (normal 1 2 ) gen
--   logInfo ("moo" :: String)  
--   put x
  

-- LoggingT / Prob

newtype T7 msg m a = T7 {
  unT7 :: LoggingT msg (Prob m) a }
     deriving (Functor, Applicative, Monad)

mkT7 h = LoggingT $ ReaderT h -- where h' msg = lift (h msg)

-- -- asdf :: ((msg -> m ()) -> m a) -> LoggingT msg m a
-- -- asdf h = LoggingT $ ReaderT $ do
  

-- |

prob1 :: PrimMonad m => Prob m Double
prob1 = do
  x <- normal 1 2
  uniformR (x, 2 * x)

prob2 :: PrimMonad m => StateT Double (Prob m) Double
prob2 = do
  x <- lift $ normal 1 2
  y <- get
  return $ x + y


-- prob3 :: PrimMonad m => LoggingT msg (StateT Double (Prob m)) Double
-- prob3 = do
--   x <- lift . lift $ normal 1 2
--   y <- lift get
--   logInfo ("moo" :: String)
--   return $ x + y

-- -- • Could not deduce (MonadLog
-- --                       (WithSeverity String) (LoggingT msg (StateT Double (Prob m))))
-- --     arising from a use of ‘logInfo’

-- -- ok smartass ghc

-- -- lemme see


prob3 ::(S.MonadState Double (Prob m), PrimMonad m) => Prob m Double
prob3 = do
  x <- normal 1 2
  y <- S.get
  let z = x + y
  S.put z
  return z


--

data P a = Normal a a | Uniform a a | Zipf a -- .. etc.

-- | Left contains the integer-valued samples, Right the Double-valued ones
interpP :: (PrimMonad m, Integral a) => P Double -> Prob m (Either a Double)
interpP rv = case rv of
  Normal mu sig -> Right <$> normal mu sig
  Uniform a b -> Right <$> uniformR (a, b)
  Zipf a -> Left <$> zipf a

class RV p a where
  sampleRV :: p -> Prob m a
  -- sampleRV = interpP

instance RV (P a) b where
  -- sampleRV = interpP
  

-- class RV a where
--   sampleRV :: Prob m a -> Gen (PrimState m) -> m a

-- -- | `mtl` style
-- prob4 :: (RV b, S.MonadState b m, Num b) =>
--          Prob m b
--       -> Gen (PrimState m)
--       -> m b
-- prob4 p g = do
--   x <- sampleRV p g
--   y <- S.get
--   let z = x + y
--   S.put z
--   return z




para :: (t -> ([t], b) -> b) -> b -> [t] -> b
para f b (a:as) = f a (as, para f b as)
para _ b []     = b








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
  
  
