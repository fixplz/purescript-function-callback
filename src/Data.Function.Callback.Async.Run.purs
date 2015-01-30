module Data.Function.Callback.Async.Run where

import Data.Function.Run
import Data.Function.Eff
import Data.Function.Callback
import Data.Function.Callback.Async

class AsyncRunFn fn idiomatic where
    asyncRunFn :: fn -> idiomatic

instance asyncFnEff0 :: AsyncRunFn (FnEff0 eff a) (Async eff a) where asyncRunFn fn = asyncEff (runFnEff0 fn)
instance asyncFnEff1 :: AsyncRunFn (FnEff1 eff a b) (a -> Async eff b) where asyncRunFn fn a = asyncEff (runFnEff1 fn a)
instance asyncFnEff2 :: AsyncRunFn (FnEff2 eff a b c) (a -> b -> Async eff c) where asyncRunFn fn a b = asyncEff (runFnEff2 fn a b)
instance asyncFnEff3 :: AsyncRunFn (FnEff3 eff a b c d) (a -> b -> c -> Async eff d) where asyncRunFn fn a b c = asyncEff (runFnEff3 fn a b c)
instance asyncFnEff4 :: AsyncRunFn (FnEff4 eff a b c d e) (a -> b -> c -> d -> Async eff e) where asyncRunFn fn a b c d = asyncEff (runFnEff4 fn a b c d)
instance asyncFnEff5 :: AsyncRunFn (FnEff5 eff a b c d e f) (a -> b -> c -> d -> e -> Async eff f) where asyncRunFn fn a b c d e = asyncEff (runFnEff5 fn a b c d e)

instance asyncFnCb0 :: AsyncRunFn (FnCb0 eff a) (Async eff a) where asyncRunFn fn = async (runFnCb0 fn)
instance asyncFnCb1 :: AsyncRunFn (FnCb1 eff a b) (a -> Async eff b) where asyncRunFn fn a = async (runFnCb1 fn a)
instance asyncFnCb2 :: AsyncRunFn (FnCb2 eff a b c) (a -> b -> Async eff c) where asyncRunFn fn a b = async (runFnCb2 fn a b)
instance asyncFnCb3 :: AsyncRunFn (FnCb3 eff a b c d) (a -> b -> c -> Async eff d) where asyncRunFn fn a b c = async (runFnCb3 fn a b c)
instance asyncFnCb4 :: AsyncRunFn (FnCb4 eff a b c d e) (a -> b -> c -> d -> Async eff e) where asyncRunFn fn a b c d = async (runFnCb4 fn a b c d)
instance asyncFnCb5 :: AsyncRunFn (FnCb5 eff a b c d e f) (a -> b -> c -> d -> e -> Async eff f) where asyncRunFn fn a b c d e = async (runFnCb5 fn a b c d e)
