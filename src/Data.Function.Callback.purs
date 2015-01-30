module Data.Function.Callback
(
  FnCb0(..), runFnCb0,
  FnCb1(..), runFnCb1,
  FnCb2(..), runFnCb2,
  FnCb3(..), runFnCb3,
  FnCb4(..), runFnCb4,
  FnCb5(..), runFnCb5,
  AsyncEff(..),
  Callback(..),
  CallbackFunction(..),
  CallbackFunctionAsync(..),

  CbFn(..),
  NodeCb(..),
  Errorish(..)
)
where

import Data.Function
import Data.Either (Either(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

type CbFn fn val = fn (NodeCb val) Unit

newtype FnCb0 (eff :: # !) ret = FnCb0 (CbFn (Fn1) ret)
newtype FnCb1 (eff :: # !) p1 ret = FnCb1 (CbFn (Fn2 p1) ret)
newtype FnCb2 (eff :: # !) p1 p2 ret = FnCb2 (CbFn (Fn3 p1 p2) ret)
newtype FnCb3 (eff :: # !) p1 p2 p3 ret = FnCb3 (CbFn (Fn4 p1 p2 p3) ret)
newtype FnCb4 (eff :: # !) p1 p2 p3 p4 ret = FnCb4 (CbFn (Fn5 p1 p2 p3 p4) ret)
newtype FnCb5 (eff :: # !) p1 p2 p3 p4 p5 ret = FnCb5 (CbFn (Fn6 p1 p2 p3 p4 p5) ret)

foreign import data AsyncEff :: # ! -> !

type Callback cbeff val = Either Error val -> Eff cbeff Unit
type CallbackFunction eff val = forall cbeff. Callback cbeff val -> Eff (async :: AsyncEff cbeff | eff) Unit
type CallbackFunctionAsync eff val = forall eff'. Callback eff val -> Eff (async :: AsyncEff eff | eff') Unit

runFnCb0 :: forall eff ret. FnCb0 eff ret -> CallbackFunction eff ret
runFnCb0 (FnCb0 fn) cb = asEff \_ -> runFn1 fn (asCb cb)

runFnCb1 :: forall eff p1 ret. FnCb1 eff p1 ret -> p1 -> CallbackFunction eff ret
runFnCb1 (FnCb1 fn) p1 cb = asEff \_ -> runFn2 fn p1 (asCb cb)

runFnCb2 :: forall eff p1 p2 ret. FnCb2 eff p1 p2 ret -> p1 -> p2 -> CallbackFunction eff ret
runFnCb2 (FnCb2 fn) p1 p2 cb = asEff \_ -> runFn3 fn p1 p2 (asCb cb)

runFnCb3 :: forall eff p1 p2 p3 ret. FnCb3 eff p1 p2 p3 ret -> p1 -> p2 -> p3 -> CallbackFunction eff ret
runFnCb3 (FnCb3 fn) p1 p2 p3 cb = asEff \_ -> runFn4 fn p1 p2 p3 (asCb cb)

runFnCb4 :: forall eff p1 p2 p3 p4 ret. FnCb4 eff p1 p2 p3 p4 ret -> p1 -> p2 -> p3 -> p4 -> CallbackFunction eff ret
runFnCb4 (FnCb4 fn) p1 p2 p3 p4 cb = asEff \_ -> runFn5 fn p1 p2 p3 p4 (asCb cb)

runFnCb5 :: forall eff p1 p2 p3 p4 p5 ret. FnCb5 eff p1 p2 p3 p4 p5 ret -> p1 -> p2 -> p3 -> p4 -> p5 -> CallbackFunction eff ret
runFnCb5 (FnCb5 fn) p1 p2 p3 p4 p5 cb = asEff \_ -> runFn6 fn p1 p2 p3 p4 p5 (asCb cb)

-- 

foreign import asEff
  """function asEff(f) { return f }"""
  :: forall eff ret. (Unit -> ret) -> Eff eff ret

foreign import data Errorish :: *

type NodeCb val = Fn2 Errorish val Unit

asCb :: forall eff val. Callback eff val -> Fn2 Errorish val Unit

asCb cb = mkFn2 \err val ->
  let err' = castError err in
  if _isnull err'
    then runEff (cb (Right val))
    else runEff (cb (Left err'))

foreign import castError
  """
  function castError(err) {
    if(err instanceof Error) return err
    if(err) return new Error(err)
    return null
  }
  """
  :: Errorish -> Error

foreign import _isnull """function _isnull(val) { return val == null }"""
  :: forall val. val -> Boolean

foreign import runEff """function runEff(eff) { return eff() }"""
  :: forall eff val. Eff eff val -> val
