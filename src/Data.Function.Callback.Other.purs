module Data.Function.Callback.Other
(
    Cb0(), asCb0,
    Cb1(), asCb1,
    Cb2(), asCb2,
    Cb3(), asCb3,
    Cb4(), asCb4,
    Cb5(), asCb5
)
where

import Data.Function
import Control.Monad.Eff

type Cb0 = Fn0 Unit
type Cb1 a = Fn1 a Unit
type Cb2 a b = Fn2 a b Unit
type Cb3 a b c = Fn3 a b c Unit
type Cb4 a b c e = Fn4 a b c e Unit
type Cb5 a b c d e = Fn5 a b c d e Unit

asCb0 cb = mkFn0 \_ -> runEff cb
asCb1 cb = mkFn1 \a -> runEff (cb a)
asCb2 cb = mkFn2 \a b -> runEff (cb a b)
asCb3 cb = mkFn3 \a b c -> runEff (cb a b c)
asCb4 cb = mkFn4 \a b c d -> runEff (cb a b c d)
asCb5 cb = mkFn5 \a b c d e -> runEff (cb a b c d e)

foreign import runEff
  "function runEff(eff) { return eff() }"
  :: forall eff val. Eff eff val -> val
