{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Example.Project where

import Clash.Prelude
import Clash.Prelude
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Control.Monad.Trans.Class as L
import Example.MealyST

createDomain vSystem{vName="Dom50", vPeriod=hzToPeriod 50e6}

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Unsigned 8) ->
  Signal Dom50 (Unsigned 8)
topEntity = exposeClockResetEnable func

{-# ANN topEntity
  (Synthesize
    { t_name = "accum"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}

{-# OPAQUE topEntity #-}

stepM ::
  Unsigned 8 ->
  ExceptT String (StateT (Unsigned 8) Identity) (Unsigned 8)
stepM _ = do
  s <- L.lift get
  if s == 5
    then throwE "overflow" -- zu unmonad throwen
    else do
      let s' = s + 1
      L.lift (put s')
      pure s' -- passt es zur signatur an

step ::
  Unsigned 8 ->
  Unsigned 8 ->
  (Unsigned 8, Unsigned 8)
step s inp =
  unmonad s 0 (stepM inp)

func ::
  HiddenClockResetEnable dom =>
  Signal dom (Unsigned 8) ->
  Signal dom (Unsigned 8)
func =
  mealy step 0

--

--sampleN @Dom50 10 (func (pure (0 :: Unsigned 8)))
--[1,1,2,3,4,5,0,0,0,0]
-- test bench?
