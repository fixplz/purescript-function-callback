module Data.Function.Callback.Async
(
  Async(),
  async,
  runAsync,
  asyncEff,
  asAsync
)
where

import Data.Function.Run
import Data.Function.Callback
import Control.Monad.Eff (Eff())
import Data.Either (Either(..))
import Control.Monad.Eff.Exception (Error())

newtype Async eff val = Async (CallbackFunctionAsync eff val)

asAsync :: forall eff val. Async eff val -> Async eff val
asAsync = id

asyncEff :: forall eff val. Eff eff val -> Async eff val
asyncEff eff =
  Async \cb ->
    runEffLater do
      res <- eff
      cb (Right res)

async :: forall eff val. CallbackFunction eff val -> Async eff val
async cbfn = Async \cb -> unifyEff (runEffLater (cbfn cb))

runAsync :: forall eff val. Async eff val -> CallbackFunctionAsync eff val
runAsync (Async async) = async

--

instance asyncFunctor :: Functor (Async eff) where
  (<$>) f val =
    Async \cb -> runAsync val \val' -> cb (f <$> val')

instance asyncApply :: Apply (Async eff) where
  (<*>) f val =
    Async \cb ->
      runAsync f \f' ->
        joinEff $ runAsync val \val' ->
          cb (f' <*> val')

instance asyncApplicative :: Applicative (Async eff) where
  pure val = Async \cb -> runEffLater (cb (Right val))

instance asyncBind :: Bind (Async eff) where
  (>>=) val f =
    Async \cb ->
      runAsync val \val' ->
        case val' of
          Right val -> joinEff (runAsync (f val) cb)
          Left err -> cb (Left err)

instance asyncMonad :: Monad (Async eff)

--

foreign import unifyEff
  """function unifyEff(f) { return f }"""
  :: forall eff1 eff2 ret. Eff eff1 ret -> Eff eff2 ret

joinEff :: forall eff eff'. Eff (async :: AsyncEff eff | eff') Unit -> Eff eff Unit
joinEff = unifyEff

foreign import runEffLater
  """
  function runEffLater(f) {
    return function() {
      if(typeof setImmediate == 'function') setImmediate(f)
      else setTimeout(f, 0)
    }
  }
  """
  :: forall asynceff eff. Eff asynceff Unit -> Eff (async :: AsyncEff asynceff | eff) Unit
