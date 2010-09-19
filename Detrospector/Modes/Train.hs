{-# LANGUAGE
    NamedFieldPuns
  , BangPatterns  #-}
module Detrospector.Modes.Train(train) where

import Detrospector.Types
import Detrospector.Modes

import System.IO
import qualified Data.Text         as TS
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.HashMap      as H
import qualified Data.IntMap       as IM
import qualified Data.Sequence     as S
import qualified Data.Foldable     as F

-- foldl' with progress dots
progFold :: (a -> b -> a) -> a -> [b] -> IO a
progFold f = go where
  go !v []     = return v
  go !v (x:xs) = putChar '.' >> go (f v x) xs

-- Build a Markov chain with n-Char history from some input text.
train :: ModeFun
train Train{num,out} = do
  hSetBuffering stdout NoBuffering
  ys <- TL.getContents
  putStr "Calculating"
  (_,h) <- progFold (TS.foldl' roll) (emptyQ,H.empty) $ TL.toChunks ys
  putStrLn "done."
  writeChain out . Chain num $ H.map cumulate h where

  roll (!s,!h) x
    = (shift num x s, F.foldr (H.alter $ ins x) h $ S.tails s)

  ins x Nothing  = Just $! sing x
  ins x (Just v) = Just $! incr x v

  sing x = IM.singleton (fromEnum x) 1

  incr x = IM.alter f $ fromEnum x where
    f Nothing  = Just 1
    f (Just v) = Just $! (v+1)

train _ = error "impossible: wrong mode passed to train"
