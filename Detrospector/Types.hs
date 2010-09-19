{-# LANGUAGE
    ViewPatterns
  , PatternGuards #-}
{-# OPTIONS_GHC
  -fno-warn-orphans #-}
module Detrospector.Types(
    FreqTable
  , PickTable
  , cumulate
  , RNG
  , Queue, emptyQ, listToQueue, queueToList, qLength, qSnoc
  , shift
  , Chain(..), pick
  , withChain, writeChain) where

import Data.Maybe
import Data.List
import Control.Applicative
import qualified Data.HashMap           as H
import qualified Data.Hashable          as H
import qualified Data.IntMap            as IM
import qualified System.Random.MWC      as RNG
import qualified Data.Sequence          as S
import qualified Data.Foldable          as F
import qualified Data.Binary            as Bin
import qualified Data.ByteString.Lazy   as BSL
import qualified Codec.Compression.GZip as Z

-- This table records character frequency;
-- each Int key is the codepoint of a Char.
type FreqTable = IM.IntMap Int

-- A table for efficiently sampling a finite
-- discrete distribution of Char.
--
-- To sample from (PickTable n im):
-- * Pick random k from [0,n) uniformly
-- * Take value of first key > k
data PickTable = PickTable Int (IM.IntMap Char)
  deriving (Show)

cumulate :: FreqTable -> PickTable
cumulate t = PickTable r $ IM.fromList ps where
  (r,ps) = mapAccumR f 0 $ IM.assocs t
  f ra (x,n) = let rb = ra+n in (rb, (rb, toEnum x))

type RNG = RNG.GenIO

-- Sample from a PickTable.
sample :: PickTable -> RNG -> IO Char
sample (PickTable t im) g = do
  k <- (`mod` t) <$> RNG.uniform g
  case IM.split k im of
    -- last key in im is t, and we know k < t
    -- therefore the right list cannot be empty
    (_, IM.toList -> ((_,x):_)) -> return x
    _ -> error "impossible"

type Queue a = S.Seq a

emptyQ :: Queue a
emptyQ = S.empty

listToQueue :: [a] -> Queue a
listToQueue = S.fromList

queueToList :: Queue a -> [a]
queueToList = F.toList

qLength :: Queue a -> Int
qLength = S.length

qSnoc :: Queue a -> a -> Queue a
qSnoc = (S.|>)

-- Enqueue at one side while dropping from the other
shift :: Int -> a -> Queue a -> Queue a
shift n x q
  | S.length q < n          = q S.|> x
  | (_ S.:< s) <- S.viewl q = s S.|> x
  | otherwise               = q S.|> x

-- The Markov chain itself.
-- (Chain n hm) maps subsequences of up to n Chars to finite
-- Char distributions represented by PickTables in hm.
data Chain = Chain Int (H.HashMap (Queue Char) PickTable)
  deriving (Show)

-- Pick from a chain according to history.  Returns a character
-- and the amount of history actually used.
pick :: Chain -> Queue Char -> RNG -> IO (Char, Int)
pick (Chain _ h) s g = do x <- sample pt g; return (x,hn) where
  (pt, hn) = get s

  -- assumption: map is nonempty for empty key
  get  t = fromMaybe (get $ qTail t) look where
    qTail (S.viewl -> (_ S.:< r)) = r
    qTail _ = error "qTail: empty queue"
    look = do x <- H.lookup t h; return (x, S.length t)

-- orphan instance: make Seq hashable
instance (H.Hashable a) => H.Hashable (S.Seq a) where
  {-# SPECIALIZE instance H.Hashable (S.Seq Char) #-}
  hash = F.foldl' (\acc h -> acc `H.combine` H.hash h) 0

-- orphan instance: Binary serialization of HashMap
instance (Bin.Binary k, Bin.Binary v, H.Hashable k, Ord k)
       => Bin.Binary (H.HashMap k v) where
  put = Bin.put . H.assocs
  get = H.fromList <$> Bin.get
  
instance Bin.Binary PickTable where
  put (PickTable n t) = Bin.put (n,t)
  get = uncurry PickTable <$> Bin.get

instance Bin.Binary Chain where
  put (Chain n h) = Bin.put (n,h)
  get = uncurry Chain <$> Bin.get

withChain :: FilePath -> (Chain -> RNG -> IO a) -> IO a
withChain p f = do
  ch <- (Bin.decode . Z.decompress) <$> BSL.readFile p
  RNG.withSystemRandom $ f ch

writeChain :: FilePath -> Chain -> IO ()
writeChain out = BSL.writeFile out . Z.compress . Bin.encode
