{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MealyST where

import Clash.Prelude
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Select
import Data.Functor.Identity
import qualified Control.Monad.Trans.Class as L

class Monad m => MealyState s m where
  getM :: m s
  putM :: s -> m ()
  stateM :: (s -> (a,s)) -> m a

class Monad m => MonadHandler m s | m -> s where
  unmonad :: s -> o -> m o -> (s, o) -- s is initial state, o default output

--

instance MealyState s (StateT s (MaybeT Identity)) where
  getM = get
  putM = put
  stateM = state

runStMa :: StateT s (MaybeT Identity) a -> s -> Maybe (a, s)
runStMa m s = runIdentity (runMaybeT (runStateT m s))

instance MonadHandler (StateT s (MaybeT Identity)) s where
  unmonad s def m =
    case runStMa m s of
      Nothing      -> (s, def) -- rollback auf alten state
      Just (o,s')  -> (s', o)

--

instance MealyState s (MaybeT (StateT s Identity)) where
  getM = L.lift get
  putM = L.lift . put
  stateM = L.lift . state

runMaSt :: MaybeT (StateT s Identity) a -> s -> (Maybe a, s)
runMaSt m s = runIdentity (runStateT (runMaybeT m) s)

instance MonadHandler (MaybeT (StateT s Identity)) s where
  unmonad s def m =
    let (res, s') = runMaSt m s
    in case res of
         Nothing -> (s', def) -- neuer state bleibt trotz fehler
         Just o  -> (s', o)

--

instance Monoid w => MealyState s (StateT s (AccumT w Identity)) where
  getM = get
  putM = put
  stateM = state

runStAc :: StateT s (AccumT w Identity) a -> s -> w -> ((a,s), w) -- endwert, akkum
runStAc m s w0 = runIdentity (runAccumT (runStateT m s) w0) -- w0 startwert akkum

instance Monoid w => MonadHandler (StateT s (AccumT w Identity)) s where
  unmonad s _ m =
    let ((o,s'),_) = runStAc m s mempty -- empty akkum für jeden typen
    in (s',o)
-- verwirft akkumulierten Wert w
{-
 let ((o,_),w) = runStAc m s []
    in (case w of
          [] -> s
          xs -> last xs
       , o)
-}

--

instance Monoid w => MealyState s (AccumT w (StateT s Identity)) where
  getM = L.lift get
  putM = L.lift . put
  stateM = L.lift . state

runAcSt :: AccumT w (StateT s Identity) a -> s -> w -> ((a,w),s)
runAcSt m s w0 = runIdentity (runStateT (runAccumT m w0) s)

instance Monoid w => MonadHandler (AccumT w (StateT s Identity)) s where
  unmonad s _ m =
    let ((o,_),s') = runAcSt m s mempty
    in (s', o)

--

instance MealyState s (StateT s (ExceptT e Identity)) where
  getM = get
  putM = put
  stateM = state

runStEx :: StateT s (ExceptT e Identity) a -> s -> Either e (a,s)
runStEx m s = runIdentity (runExceptT (runStateT m s))

instance MonadHandler (StateT s (ExceptT e Identity)) s where
  unmonad s def m =
    case runStEx m s of
      Left _ -> (s, def)
      Right (o,s') -> (s', o)

--

instance MealyState s (ExceptT s (StateT s Identity)) where
  getM = L.lift get
  putM = L.lift . put
  stateM = L.lift . state

runExSt :: ExceptT e (StateT s Identity) a -> s -> (Either e a, s)
runExSt m s = runIdentity (runStateT (runExceptT m) s)

instance MonadHandler (ExceptT e (StateT s Identity)) s where
  unmonad s def m =
    case runExSt m s of
      (Left _, _) -> (s, def)
      (Right o,s') -> (s', o)

--

instance Monoid w => MealyState s (StateT s (WriterT w Identity)) where
  getM = get
  putM = put
  stateM = state

runStWr :: StateT s (WriterT w Identity) a -> s -> ((a,s), w)
runStWr m s = runIdentity (runWriterT (runStateT m s))

instance Monoid w => MonadHandler (StateT s (WriterT w Identity)) s where
  unmonad s _ m =
    let ((o,s'), _) = runStWr m s -- was soll mit dem log output passieren
    in (s', o)

--

instance Monoid w => MealyState s (WriterT w (StateT s Identity)) where
  getM = L.lift get
  putM = L.lift . put
  stateM = L.lift . state

runWrSt :: WriterT w (StateT s Identity) a -> s -> ((a,w), s)
runWrSt m s = runIdentity (runStateT (runWriterT m) s)

instance Monoid w => MonadHandler (WriterT w (StateT s Identity)) s where
  unmonad s _ m =
    let ((o,_), s') = runWrSt m s
    in (s', o)

--

mealyST
  :: MonadHandler m s
  => o
  -> (i -> m o)
  -> s
  -> i
  -> (s, o)
mealyST def f s i =
  unmonad s def (f i)


-------------------- tests --------------------

fSM :: Int -> StateT Int (MaybeT Identity) Int
fSM x = do
  s <- get
  let s' = s + x
  put s'

  if x < 0
    then L.lift (MaybeT (pure Nothing))
    else pure s'
-- simulate @System (mealy (mealyyMT (999 :: Int) fSM) 0) [1,2,3,-1,5]
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
