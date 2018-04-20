{-# language OverloadedStrings, FlexibleContexts #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
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
-- * Reading configuration (optional)
-- * Logging
-- * Transforming state
-- * Sampling random variables






-- -- t :: (S.MonadState Double m, PrimMonad m) => Prob m Double
-- t = do
--   s <- lift S.get
--   w <- normal 1 2
--   lift $ S.put (s + w)
--   return s

t :: (MonadLog (WithSeverity String) m, PrimMonad m) => Prob m Double
t = do
  w <- normal 1 2
  lift $ logInfo $ show w
  return w




runT :: Monad m =>
        Handler m message
     -> StateT s (LoggingT message m) a
     -> s
     -> Int
     -> m [a]
runT h mm s0 n = runLoggingT (replicateM n $ evalStateT mm s0) h 







-- newtype T msg s m a =
--   T { unT :: LoggingT (WithSeverity msg) (StateT s (Prob m)) a }
--       deriving (Functor, Applicative, Monad)

-- instance MonadTrans (T msg s) where
--   lift = T . lift . lift . lift

-- instance MonadLog msg m => MonadLog msg (T msg s m)



-- runT :: Monad m =>
--         (WithSeverity msg -> m ())
--      -> T msg s m a
--      -> s
--      -> Gen (PrimState m)
--      -> m (a, s)
-- runT lh (T mm) x0 = sample (runStateT (runLoggingT mm lh') x0) 
--   where
--     lh' x = lift $ lift (lh x)


-- t0 :: (MonadLog String m, PrimMonad m) => T String Double m Double
-- t0 = T $ do
--   s <- lift get
--   w <- lift . lift $ normal 1 2
--   let z = s + w
--   lift . lift . lift $ logMessage (show z)
--   lift $ put z
--   return z

-- -- t :: (Monad m, Num s, PrimMonad (LoggingT (WithSeverity String) (StateT s (Prob m)))) => Gen (PrimState (LoggingT (WithSeverity String) (StateT s (Prob m)))) -> T String s m Double
-- -- t gen = T $ do
-- --   logInfo ("moo" :: String)
-- --   S.modify (+ 1)
-- --   sample (normal 1 2) gen




-- newtype SP s m a = SP {unSP :: StateT s (Prob m) a} deriving (Functor, Applicative, Monad)

-- instance MonadTrans (SP s) where
--   lift = lift

-- instance MonadLog msg m => MonadLog msg (SP s m)


-- runSP :: SP s m a -> s -> Gen (PrimState m) -> m (a, s)
-- runSP (SP ff) s0 gen = sample (runStateT ff s0) gen

-- -- runSP' h sp s0 gen = runLoggingT (runSP sp s0 gen) h' where
-- --   h' x = lift (h x)
-- --   -- hLift x = lift (h x)

-- sp0 :: (MonadLog String m, Num a, Show a) => (a -> Prob m a) -> SP a m a
-- sp0 fs = SP $ do
--   s <- get
--   w <- lift $ fs s
--   let z = w + s
--   lift . lift $ logMessage (show z)
--   put z
--   return z






-- --

-- data P a = Normal a a | Uniform a a | Zipf a -- .. etc.

-- -- | Left contains the integer-valued samples, Right the Double-valued ones
-- interpP :: (PrimMonad m, Integral a) => P Double -> Prob m (Either a Double)
-- interpP rv = case rv of
--   Normal mu sig -> Right <$> normal mu sig
--   Uniform a b -> Right <$> uniformR (a, b)
--   Zipf a -> Left <$> zipf a

-- class RV p a where
--   sampleRV :: p -> Prob m a
--   -- sampleRV = interpP

-- instance RV (P a) b where
--   -- sampleRV = interpP
  
