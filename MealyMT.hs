{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MealyMT where

import Clash.Prelude
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import qualified Control.Monad.Trans.Class as L

class Monad m => MealyState s m where
  getM :: m s
  putM :: s -> m ()
  stateM :: (s -> (a,s)) -> m a

{-
instance MealyState s (State s Identity) where
  getM = get
  putM = put
  stateM = state
-}

instance MealyState s (StateT s (MaybeT Identity)) where
  getM = get
  putM = put
  stateM = state

instance MealyState s (MaybeT (StateT s Identity)) where
  getM = L.lift get
  putM = L.lift . put
  stateM = L.lift . state

---

class Monad m => MonadHandler m s | m -> s where
  unmonad :: s -> o -> m o -> (s, o) -- s is initial state, o default output

runStMa :: StateT s (MaybeT Identity) a -> s -> Maybe (a, s)
runStMa m s = runIdentity (runMaybeT (runStateT m s))

instance MonadHandler (StateT s (MaybeT Identity)) s where
  unmonad s def m =
    case runStMa m s of
      Nothing      -> (s, def) -- rollback auf alten state
      Just (o,s')  -> (s', o)

runMaSt :: MaybeT (StateT s Identity) a -> s -> (Maybe a, s)
runMaSt m s = runIdentity (runStateT (runMaybeT m) s)

instance MonadHandler (MaybeT (StateT s Identity)) s where
  unmonad s def m =
    let (res, s') = runMaSt m s
    in case res of
         Nothing -> (s', def) -- neuer state bleibt trotz fehler
         Just o  -> (s', o)

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
