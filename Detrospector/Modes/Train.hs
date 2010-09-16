{-# LANGUAGE
    NamedFieldPuns
  , BangPatterns  #-}
module Detrospector.Modes.Train(train) where

import Detrospector.Types
import Detrospector.Modes

import qualified Data.Text.Lazy    as Txt
import qualified Data.Text.Lazy.IO as Txt
import qualified Data.HashMap      as H
import qualified Data.IntMap       as IM
import qualified Data.Sequence     as S
import qualified Data.Foldable     as F

-- Build a Markov chain with n-Char history from some input text.
train :: ModeFun
train Train{num,out} = do
  ys <- Txt.getContents
  let hm = H.map cumulate . snd $ Txt.foldl' roll (emptyQ,H.empty) ys
  writeChain out $ Chain num hm where

  roll (!s,!h) x
    = (shift num x s, F.foldr (H.alter $ ins x) h $ S.tails s)

  ins x Nothing  = Just $! sing x
  ins x (Just v) = Just $! incr x v

  sing x = IM.singleton (fromEnum x) 1

  incr x = IM.alter f $ fromEnum x where
    f Nothing  = Just 1
    f (Just v) = Just $! (v+1)

train _ = error "impossible: wrong mode passed to train"
