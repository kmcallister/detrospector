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

-- The guts of a Chain, before we apply 'cumulate'.
type FreqChain = H.HashMap (S.Seq Char) FreqTable

-- Build a Markov chain with n-Char history from some input text.
train :: ModeFun
train Train{num,out} = do
  hSetBuffering stdout NoBuffering
  ys <- TL.getContents
  putStr "Calculating"
  (_,h) <- progFold (TS.foldl' roll) (emptyQ,H.empty) $ TL.toChunks ys
  putStrLn "done."
  writeChain out . Chain num $ H.map cumulate h where

  -- Process another character, updating a fold state of the Markov chain
  -- history and the accumulated FreqChain.
  roll :: (S.Seq Char, FreqChain) -> Char -> (S.Seq Char, FreqChain)
  roll (!s, !fci) x = (shift num x s, F.foldr occur fci $ S.tails s) where

    -- Increment the occurrence count for 'x' following history 'hist'.
    occur :: S.Seq Char -> FreqChain -> FreqChain
    occur hist fc = H.insert hist (bump $ H.lookup hist fc) fc

    -- Given a FreqTable (or Nothing), return a FreqTable with one more
    -- count for 'x'.
    bump :: Maybe FreqTable -> FreqTable
    bump Nothing  = IM.singleton (fromEnum x) 1
    bump (Just v) = IM.alter incMaybe (fromEnum x) v

    incMaybe :: Maybe Int -> Maybe Int
    incMaybe Nothing  = Just 1
    incMaybe (Just n) = Just $! (n+1)

train _ = error "impossible: wrong mode passed to train"
