{-# LANGUAGE
    NamedFieldPuns
  , BangPatterns  #-}
module Detrospector.Modes.Train(train) where

import Detrospector.Types
import Detrospector.Modes

import System.IO
import qualified Data.Text           as TS
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap         as IM
import qualified Data.Sequence       as S
import qualified Data.Foldable       as F

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

  roll (!s,!h) x = (shift num x s, F.foldr alter h $ S.tails s) where

    alter k hm = H.insert k (ins $ H.lookup k hm) hm

    ins Nothing  = IM.singleton (fromEnum x) 1
    ins (Just v) = IM.alter inc (fromEnum x) v

    inc Nothing  = Just 1
    inc (Just n) = Just $! (n+1)

train _ = error "impossible: wrong mode passed to train"
