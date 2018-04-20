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
import Control.Monad.Trans.State.Strict (StateT(..), get, put, evalStateT, execStateT, runStateT)
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Primitive
import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, renderWithSeverity, withFDHandler, defaultBatchingOptions, logMessage, logDebug, logInfo, logNotice, logWarning, logError)

import Text.PrettyPrint.Leijen.Text.Doc

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



-- | This works
--
-- > :t create >>= \g -> runLoggingT (mtl3 g) print
-- > :: IO Double
mtl3 :: (MonadTrans t, PrimMonad m, MonadLog (WithSeverity String) (t m)) =>
        Gen (PrimState m)
     -> t m Double
mtl3 gen = do
  w <- lift $ sample (normal 1 2) gen
  logInfo $ show w
  return w



--



mtl4 :: (S.MonadState s m, MonadLog (WithSeverity a) (t m), MonadTrans t) =>
        (s -> Prob m p)
     -> (s -> p -> m (a, s))
     -> (a -> s -> a)
     -> Gen (PrimState m)
     -> t m a
mtl4 fm fs flog gen = do
  s <- lift S.get
  w <- lift $ sample (fm s) gen
  (a, s') <- lift $ fs s w
  logInfo $ flog a s' 
  lift $ S.put s'
  return a

data T4 s p m a = T4 {
    t4model :: s -> Prob m p
  , t4StateFunc :: a -> p -> m (a, s)
  , t4logHdlr :: Handler m (WithSeverity a)
  , t4Funct :: a -> s -> a
  }

-- runT4 (T4 mm sf f) g 


-- runMtl41 :: Monad m =>
--             (s -> Prob (StateT s m) p)
--          -> (s -> p -> StateT s m (a, s))
--          -> Handler m (WithSeverity a)
--          -> (a -> s -> a)
--          -> s
--          -> Gen (PrimState m)
--          -> m (a, s)
runMtl41 model fstate logf ff s0 gen =
  runStateT (runLoggingT (mtl4 model fstate ff gen) (lift . logf) ) s0


-- | Usage :
--
-- > runMtl4 (const $ normal 1 2) (\ _ _ -> pure (1, 2)) print (+) 0 create
runMtl4 :: Monad m =>
           (s -> Prob (StateT s m) p)
        -> (s -> p -> StateT s m (a, s))
        -> (WithSeverity a -> m ())
        -> (a -> s -> a)
        -> s
        -> m (Gen (PrimState m))
        -> m (a, s)
runMtl4 model fstate logf ff s0 gen = flip runStateT s0 $ do
  g <- lift gen
  runLoggingT (mtl4 model fstate ff g) (lift . logf)
  


-- | T5 

newtype T5 msg s m a = T5 (LoggingT msg (StateT s m) a)

mtl5 :: Monad m =>
        (s -> Prob m t)
     -> (s -> t -> (a, s))
     -> (a -> s -> message)
     -> Gen (PrimState m)
     -> T5 message s m a
mtl5 fm fs flog gen = T5 $ do
  s <- lift S.get
  w <- lift . lift $ sample (fm s) gen
  let (a, s') = fs s w
  logMessage $ flog a s' 
  lift $ S.put s'
  return a

runT5 :: Monad m =>
         Handler m msg
      -> T5 msg s m a2
      -> s
      -> m (a2, s)
runT5 logf (T5 mm) = runStateT (runLoggingT mm (lift . logf) )


  
-- | T6

newtype T6 msg s m a = T6 {
  sampleT6 :: Gen (PrimState m) -> LoggingT msg (StateT s m) a } deriving (Functor)

mkT6 :: Monad m =>
        (s -> Prob m t)
     -> (s -> t -> (a, s))
     -> (a -> s -> message)
     -> T6 message s m a
mkT6 fm fs flog = T6 $ \gen -> do
  s <- lift S.get
  w <- lift . lift $ sample (fm s) gen
  let (a, s') = fs s w
  logMessage $ flog a s' 
  lift $ S.put s'
  return a

runT6 :: Monad m =>
         Handler m msg
      -> T6 msg s m a
      -> s
      -> Gen (PrimState m)
      -> m (a, s)
runT6 logf (T6 fm) s0 g = runStateT (runLoggingT (fm g) (lift . logf)) s0




-- | T7

newtype T7 msg s m a = T7 {
  sampleT7 :: Gen (PrimState m) -> StateT s (LoggingT msg m) a } deriving (Functor)

mkT7 :: Monad m =>
        (s -> Prob m t)
     -> (s -> t -> (a, s))
     -> (a -> s -> message)
     -> T7 message s m a
mkT7 fm fs flog = T7 $ \gen -> do
  s <- S.get
  w <- lift . lift $ sample (fm s) gen
  let (a, s') = fs s w
  lift $ logMessage $ flog a s' 
  S.put s'
  return a

runT7 :: Monad m =>
         Handler m message
      -> T7 message s m a
      -> Int
      -> s
      -> Gen (PrimState m)
      -> m [(a, s)]
runT7 logf (T7 fm) n s0 g =
  runLoggingT (replicateM n (runStateT (fm g) s0)) logf

  









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
  
