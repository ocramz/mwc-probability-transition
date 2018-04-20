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
        (WithSeverity msg -> m ())
     -> T msg s m a
     -> s
     -> Gen (PrimState m)
     -> m (a, s)
runT lh (T mm) x0 gen = sample (runStateT (runLoggingT mm lh') x0) gen
  where
    lh' x = lift $ lift (lh x)


t0 :: (MonadLog String m, PrimMonad m) => T String Double m Double
t0 = T $ do
  s <- lift get
  w <- lift . lift $ normal 1 2
  let z = s + w
  lift . lift . lift $ logMessage (show z)
  lift $ put z
  return z

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

t2 :: (Monad m, PrimMonad (LoggingT msg (Prob m))) =>
     Gen (PrimState (LoggingT msg (Prob m))) -> T2 msg s m ()
t2 gen = T2 $ do
  x <- S.get
  w <- lift $ sample (normal 1 2) gen
  S.put x



-- | mtl : MonadState + MonadLog

t3 :: (S.MonadState s m, MonadLog (WithSeverity String) m) => m ()
t3 = do
  x <- S.get
  logInfo ("moo" :: String)
  S.put x

  

-- | mtl : StateT / Prob + MonadProb typeclass

newtype T4 s m a = T4 { unT4 :: StateT s (Prob m) a }

class MonadProb m a where
  mpSample :: Prob m a -> Gen (PrimState m) -> m a
  mpSample = sample  

t4 :: (S.MonadState b m, MonadProb m a, MonadLog (WithSeverity String) m) =>
     Prob m a -> Gen (PrimState m) -> m b
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


-- | Same thing as t6, only using mtl MonadState rather than transformers StateT
--
-- NB: IO does not have a MonadLog (..) instance
t6' :: (S.MonadState b m, MonadLog (WithSeverity a) m) =>
     (b -> Prob m t)
     -> (t -> b -> b)
     -> (b -> WithSeverity a)
     -> Gen (PrimState m)
     -> m b
t6' mf stf logf gen = do
  s <- S.get
  w <- sample (mf s) gen
  let s' = stf w s
      le@(WithSeverity _ _) = logf s'
  logMessage le
  S.put s'
  return s'


{- |
 NB :
  MonadIO m => MonadIO (LoggingT message m)
  MonadIO m => MonadIO (StateT s m)
-}

newtype L msg s m a = L (StateT s (LoggingT msg m) a) deriving (Functor, Applicative, Monad, MonadIO)

runL :: Handler m message -> L message s m a -> s -> m (a, s)
runL msgHdl (L ms) x0 = runLoggingT (runStateT ms x0) msgHdl


lTest :: MonadIO m => L String Int m Int
lTest = L $ do
  s <- get
  logMessage "moo"
  liftIO $ putStrLn "hi from IO"
  put s
  return s



-- newtype M msg s m a = M (StateT s (LoggingT msg (Prob m)) a) deriving (Functor, Applicative, Monad)

-- runM h (M mm) s0 = sample (runLoggingT (runStateT mm s0) h)

newtype N msg s m a = N (StateT s (Prob (LoggingT msg m)) a) deriving (Functor, Applicative, Monad)

runN :: Handler m message
     -> N message s m a
     -> s
     -> Gen (PrimState (LoggingT message m))
     -> m (a, s)
runN h (N mm) s0 gen = runLoggingT (sample (runStateT mm s0) gen) h 

-- nTest :: Monad m => N String Int m Int
-- nTest = N $ do
--   s <- get
--   logMessage "moo"
--   put s
--   return s


newtype M msg s m a = M {
  unM :: LoggingT msg (StateT s m) a } deriving (Functor, Applicative, Monad)

mTest :: Monad m => (Int -> Prob m a2) -> Gen (PrimState m) -> M String Int m Int
mTest fs gen = M $ do
  s <- lift get
  logMessage "moo"
  w <- lift . lift $ sample (fs s) gen
  lift $ put s
  return s



m1 :: MonadLog message m =>     
      (s -> Prob m p)
   -> (s -> p -> (a, s))
   -> (s -> a -> message)
   -> Gen (PrimState m)
   -> M message s m a
m1 fs f flog gen = M $ do
  s <- lift get
  w <- lift . lift $ sample (fs s) gen
  let (a, s') = f s w
  logMessage (flog s' a)
  lift $ put s'
  return a

runM :: Monad m => Handler m message -> M message s m a -> s -> m (a, s)
runM h (M mm) s0 = runStateT (runLoggingT mm hLift) s0 where
  hLift x = lift (h x)

-- NOTE :
-- 
-- λ> :t \pf gen -> runM pf (m1 (\_ -> normal 1 2) (\ s p -> (s, p)) (\_ _ -> "moo") gen)
-- \pf gen -> runM pf (m1 (\_ -> normal 1 2) (\ s p -> (s, p)) (\_ _ -> "moo") gen)
--   :: (PrimMonad m, MonadLog [Char] m) =>
--      Handler m [Char]
--      -> Gen (PrimState m) -> Double -> m (Double, Double)

m2 :: (S.MonadState s m, MonadLog message m) =>
      (b -> s -> message)
   -> (s -> Prob m t)
   -> (s -> t -> (b, s))
   -> Gen (PrimState m)
   -> m b
m2 flog fs f gen = do
  s <- S.get
  w <- sample (fs s) gen
  let (a, s') = f s w
  logMessage (flog a s')
  S.put s'
  return a




newtype SP s m a = SP {unSP :: StateT s (Prob m) a} deriving (Functor, Applicative, Monad)

instance MonadTrans (SP s) where
  lift = lift

instance MonadLog msg m => MonadLog msg (SP s m)


runSP :: SP s m a -> s -> Gen (PrimState m) -> m (a, s)
runSP (SP ff) s0 gen = sample (runStateT ff s0) gen

-- runSP' h sp s0 gen = runLoggingT (runSP sp s0 gen) h' where
--   h' x = lift (h x)
--   -- hLift x = lift (h x)

sp0 :: (MonadLog String m, Num a, Show a) => (a -> Prob m a) -> SP a m a
sp0 fs = SP $ do
  s <- get
  w <- lift $ fs s
  let z = w + s
  lift . lift $ logMessage (show z)
  put z
  return z







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
  
  
