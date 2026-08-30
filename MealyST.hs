{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Example.MealyST where

import qualified Prelude as P
import Clash.Prelude
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State hiding (get, put, state)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import Data.Functor.Identity
import qualified Control.Monad.Trans.Class as L
import Data.Monoid (Sum(..))

import Control.Monad.Except (MonadError, throwError)

class Monad m => StateComb m s | m -> s where
  runComb :: s -> o -> m o -> (s, o) -- s is initial state, o default output

-- ============================================================
-- StateT s (MaybeT Identity)
-- ============================================================

runStMa
  :: StateT s (MaybeT Identity) a
  -> s
  -> Maybe (a, s)
runStMa m s =
  runIdentity (runMaybeT (runStateT m s))

instance StateComb (StateT s (MaybeT Identity)) s where
  runComb s def m =
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

instance StateComb (MaybeT (StateT s Identity)) s where
  runComb s def m =
    let (res, s') = runMaSt m s
    in case res of
         Nothing -> (s', def) -- new state stays despite error
         Just o  -> (s', o)

-- ============================================================
-- StateT s (ExceptT e Identity)
-- ============================================================

runStEx
  :: StateT s (ExceptT e Identity) a
  -> s
  -> Either e (a,s)
runStEx m s =
    runIdentity (runExceptT (runStateT m s))

instance StateComb (StateT s (ExceptT e Identity)) s where
  runComb s def m =
    case runStEx m s of
      Left _ -> (s, def) -- rollback
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

instance StateComb (ExceptT e (StateT s Identity)) s where
  runComb s def m =
    case runExSt m s of
      (Left _, s') -> (s', def)
      (Right o,s') -> (s', o)

-- ============================================================
-- StateT s (MaybeT (ExceptT e Identity))
-- ============================================================

runStMaEx
  :: StateT s (MaybeT (ExceptT e Identity)) a
  -> s
  -> Either e (Maybe (a, s))
runStMaEx m s =
    runIdentity $ runExceptT $ runMaybeT $ runStateT m s

instance StateComb (StateT s (MaybeT (ExceptT e Identity))) s where
  runComb s def m =
    case runStMaEx m s of
      Left _ ->
        (s, def)
      Right Nothing ->
        (s, def)
      Right (Just (o, s')) ->
        (s', o)

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
    => StateComb (StateT s (AccumT w Identity)) s where
  runComb s _ m =
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
    => StateComb (AccumT w (StateT s Identity)) s where
  runComb s _ m =
    let ((o, _),s') = runAcSt m s mempty
    in (s', o)

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
    => StateComb (StateT s (WriterT w Identity)) s where
  runComb s _ m =
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

instance StateComb (WriterT (Sum Int) (StateT Int Identity)) Int where
  runComb s _ m =
    let ((o, Sum delta), s') = runWrSt m s
    in (s' + delta, o)

-- ============================================================
-- Mealy machine
-- ============================================================

mealyST
  :: StateComb m s
  => o
  -> (i -> m o)
  -> s
  -> i
  -> (s, o)
mealyST def f s i =
  runComb s def (f i)

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
-- simulate @System (mealy (mealyST (999 :: Int) fSM) 0) [1,2,3,-1,5]
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

f :: (MonadState Int m, MonadError () m) => Int -> m Int
f x = do
  s <- get
  let s' = s + x
  put s'

  if x < 0
    then throwError ()
    else pure s'

--------

fWS :: Int -> WriterT (Sum Int) (StateT Int Identity) Int
fWS x = do
  s <- L.lift get
  tell (Sum x)
  pure (s + x)

-- input is state delta
--simulate @System (mealy (mealyST (999 :: Int) fWS) 0) [1,2,3,-1,5]
--[1,3,6,5,10] state changed by the logged delta Sum value each cycle

-- ============================================================
-- Evaluation (Project.hs)
-- ============================================================

{-
WriterT [Event] (MaybeT (StateT s Identity)) a

vS

MaybeT (WriterT [Event] (StateT s Identity)) a

Do events produced before a failure survive the failure?

do
  tell [Started]
  modify (+1)
  tell [Incremented]
  throwError ...

state changes: rolled back
events: rolled back

state changes: rolled back
events: preserved

state changes: preserved
events: preserved

event log
state:
    old state ────────────────> old state

log:
    Started
    Incremented
    Failed
-}
