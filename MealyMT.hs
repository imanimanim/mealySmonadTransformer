module MealyMT where

import Clash.Prelude
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import qualified Control.Monad.Trans.Class as L

class Monad m => MonadHandler m where
  unmonad :: s -> o -> m (o, s) -> (o, s) -- s is initial state, o default output

newtype StMa s a = StMa { unStMa :: StateT s (MaybeT Identity) a } -- unwrapper fct
  deriving (Functor, Applicative, Monad) -- Maybe (s, o)

newtype MaSt s a = MaSt { unMaSt :: MaybeT (StateT s Identity) a }
  deriving (Functor, Applicative, Monad) -- (s, Maybe o)

runStMa ::StMa s a -> s -> Maybe (a, s)
runStMa (StMa m) s = runIdentity (runMaybeT (runStateT m s))

runMaSt :: MaSt s a -> s -> (Maybe a, s)
runMaSt (MaSt m) s = runIdentity (runStateT (runMaybeT m) s)

instance MonadHandler (StMa s) where
  unmonad s def m =
    case runStMa m s of
      Nothing      -> (def, s) -- def o, start state
      Just (o,s')  -> (o, s')

instance MonadHandler (MaSt s) where
  unmonad s def m =
    let (res, s') = runMaSt m s
    in case res of
         Nothing -> (def, s') -- default o, next state
         Just o  -> (o, s')

-- tests für StMa MaSt

fStMa :: Int -> StateT Int (MaybeT Identity) Int
fStMa i = StateT $ \s ->
  if i == 3
    then Nothing
    else
      let s' = s + i
      in Just (s', s')

fMaSt :: Int -> StateT Int (MaybeT Identity) Int
fMaSt = undefined

testStMa = scanl (\(o,s) i -> mealyMT 0 fStMa s i) (0,0) inputs
testMaSt = scanl (\(o,s) i -> mealyMT 0 fMaSt s i) (0,0) inputs

initState :: Int
initState = 0

inputs :: [Int]
inputs = [1,2,3,4,5]
-------------------------------------------------


-- mealy fkt noch verändern
mealyMT -- step fct for mealy
  :: MonadHandler m
  => o -- default thing
  -> (i -> StateT s m o)
  -> s
  -> i
  -> (o, s)
mealyMT def f s i =
  unmonad s def (runStateT (f i) s)
