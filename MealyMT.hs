{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Example.MealyT where

import Clash.Prelude
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State hiding (get, put, state)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import Data.Functor.Identity
import qualified Control.Monad.Trans.Class as L

class Monad m => MonadHandler m s | m -> s where
  unmonad :: s -> o -> m o -> (s, o) -- s is initial state, o default output

-- ============================================================
-- StateT s (MaybeT Identity)
-- ============================================================

runStMa
  :: StateT s (MaybeT Identity) a
  -> s
  -> Maybe (a, s)
runStMa m s =
  runIdentity (runMaybeT (runStateT m s))

instance MonadHandler (StateT s (MaybeT Identity)) s where
  unmonad s def m =
    case runStMa m s of
      Nothing      -> (s, def) -- rollback to old state
      Just (o,s')  -> (s', o)

-- ============================================================
-- MaybeT (StateT s Identity)
-- ============================================================

runMaSt
  :: MaybeT (StateT s Identity) a
  -> s
  -> (Maybe a, s)
runMaSt m s =
  runIdentity (runStateT (runMaybeT m) s)

instance MonadHandler (MaybeT (StateT s Identity)) s where
  unmonad s def m =
    let (res, s') = runMaSt m s
    in case res of
         Nothing -> (s', def) -- new state stays despite error
         Just o  -> (s', o)

-- ============================================================
-- StateT s (AccumT w Identity)
-- ============================================================

runStAc
  :: Monoid w
  => StateT s (AccumT w Identity) a
  -> s
  -> w
  -> ((a,s), w) -- final value, accum
runStAc m s w0 =
  runIdentity (runAccumT (runStateT m s) w0) -- w0 is staring value of the accum

instance Monoid w
    => MonadHandler (StateT s (AccumT w Identity)) s where
  unmonad s _ m =
    let ((o,s'),_) = runStAc m s mempty -- empty accum for every type
    in (s',o)

-- ============================================================
-- AccumT w (StateT s Identity)
-- ============================================================

runAcSt
  :: Monoid w
  => AccumT w (StateT s Identity) a
  -> s
  -> w
  -> ((a,w),s)
runAcSt m s w0 =
  runIdentity (runStateT (runAccumT m w0) s)

instance Monoid w
    => MonadHandler (AccumT w (StateT s Identity)) s where
  unmonad s _ m =
    let ((o,_),s') = runAcSt m s mempty
    in (s', o)

-- ============================================================
-- StateT s (ExceptT e Identity)
-- ============================================================

runStEx
  :: StateT s (ExceptT e Identity) a
  -> s
  -> Either e (a,s)
runStEx m s =
    runIdentity (runExceptT (runStateT m s))

instance MonadHandler (StateT s (ExceptT e Identity)) s where
  unmonad s def m =
    case runStEx m s of
      Left _ -> (s, def)
      Right (o,s') -> (s', o)

-- ============================================================
-- ExceptT e (StateT s Identity)
-- ============================================================

runExSt
  :: ExceptT e (StateT s Identity) a
  -> s
  -> (Either e a, s)
runExSt m s =
    runIdentity (runStateT (runExceptT m) s)

instance MonadHandler (ExceptT e (StateT s Identity)) s where
  unmonad s def m =
    case runExSt m s of
      (Left _, _) -> (s, def) -- rollback
      (Right o,s') -> (s', o)

-- ============================================================
-- StateT s (WriterT w Identity)
-- ============================================================

runStWr
  :: Monoid w
  => StateT s (WriterT w Identity) a
  -> s
  -> ((a,s), w)
runStWr m s =
  runIdentity (runWriterT (runStateT m s))

instance Monoid w
    => MonadHandler (StateT s (WriterT w Identity)) s where
  unmonad s _ m =
    let ((o,s'), _) = runStWr m s
    in (s', o)

-- ============================================================
-- WriterT w (StateT s Identity)
-- ============================================================

runWrSt
  :: Monoid w
  => WriterT w (StateT s Identity) a
  -> s
  -> ((a,w), s)
runWrSt m s =
  runIdentity (runStateT (runWriterT m) s)

instance Monoid w
    => MonadHandler (WriterT w (StateT s Identity)) s where
  unmonad s _ m =
    let ((o,_), s') = runWrSt m s
    in (s', o)

-- ============================================================
-- Mealy machine
-- ============================================================

mealyT
  :: MonadHandler m s
  => o
  -> (i -> m o)
  -> s
  -> i
  -> (s, o)
mealyT def f s i =
  unmonad s def (f i)

-- ============================================================
-- Tests
-- ============================================================

fSM :: Int -> StateT Int (MaybeT Identity) Int
fSM x = do
  s <- get
  let s' = s + x
  put s'

  if x < 0
    then L.lift (MaybeT (pure Nothing))
    else pure s'
-- simulate @System (mealy (mealyT (999 :: Int) fSM) 0) [1,2,3,-1,5]
-- [1,3,6,999,11] nach dem fehler wird wieder die 6 als state genommen

fMS :: Int -> MaybeT (StateT Int Identity) Int
fMS x = do
  s <- L.lift get
  let s' = s + x
  L.lift (put s')
  if x < 0
    then MaybeT (pure Nothing)
    else pure s'
-- [1,3,6,999,10]
-- 6 - 1 wird berechnet
