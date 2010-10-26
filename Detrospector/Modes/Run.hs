{-# LANGUAGE
    NamedFieldPuns #-}
module Detrospector.Modes.Run(run) where

import Detrospector.Types
import Detrospector.Modes

go :: Chain -> RNG -> IO ()
go c@(Chain n _) g = loop emptyQ where
  loop s = do
    (x,_) <- pick c s g
    putChar x
    loop $! shift n x s

run :: ModeFun
run Run{chain} = withChain chain go
run _ = error "impossible: wrong mode passed to run"
