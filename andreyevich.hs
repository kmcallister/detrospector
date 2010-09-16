{-# LANGUAGE
    BangPatterns
  , ViewPatterns
  , PatternGuards
  , DeriveDataTypeable
  , NamedFieldPuns #-}
module Main(main) where

import Data.List
import Data.Maybe
import Control.Applicative
import Data.Typeable(Typeable)
import Data.Data    (Data    )
import qualified Data.HashMap           as H
import qualified Data.Hashable          as H
import qualified Data.IntMap            as IM
import qualified System.Random.MWC      as RNG
import qualified Data.Sequence          as S
import qualified Data.Foldable          as F
import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)))
import qualified Data.Binary            as Bin
import qualified Data.ByteString.Lazy   as BSL
import qualified Codec.Compression.GZip as Z
import qualified Data.Text.Lazy         as Txt
import qualified Data.Text.Lazy.IO      as Txt

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

type Queue a = S.Seq a

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

-- orphan instance: make Seq hashable
instance (H.Hashable a) => H.Hashable (S.Seq a) where
  {-# SPECIALIZE instance H.Hashable (S.Seq Char) #-}
  hash = F.foldl' (\acc h -> acc `H.combine` H.hash h) 0

-- Build a Markov chain with n-Char history from some input text.
makeChain :: Int -> Txt.Text -> Chain
makeChain n ys = Chain n hm where
  hm = H.map cumulate . snd $ Txt.foldl' roll (S.empty,H.empty) ys

  roll (!s,!h) x
    = (shift n x s, F.foldr (H.alter $ ins x) h $ S.tails s)

  ins x Nothing  = Just $! sing x
  ins x (Just v) = Just $! incr x v

  sing x = IM.singleton (fromEnum x) 1

  incr x = IM.alter f $ fromEnum x where
    f Nothing  = Just 1
    f (Just v) = Just $! (v+1)

-- Run a Markov chain, printing output forever.
runChain :: Chain -> RNG.GenIO -> IO ()
runChain (Chain n h) g = go S.empty where
  go s = do
    x <- pick $ get s
    putChar x
    go $! shift n x s

  get s = fromMaybe (get $ stail s) $ H.lookup s h where
    stail (S.viewl -> (_ S.:< t)) = t

  pick (PickTable t im) = do
    k <- (`mod` t) <$> RNG.uniform g
    case IM.split k im of
      -- last key in im is t, and we know k < t
      -- therefore the right list cannot be empty
      (_, IM.toList -> ((_,x):_)) -> return x
      _ -> error "impossible"

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

data Mode
  = Train   { num      :: Int
            , out      :: FilePath }
  | Run     { chain    :: FilePath }
  deriving (Show, Typeable, Data)

train, run, modes :: Annotate Arg.Ann

train = Arg.record Train{num=undefined, out=undefined}
  [ num := 3
        += Arg.help "Number of characters lookback"
  , out := error "Must specify output chain"
        += Arg.typFile
        += Arg.help "Write chain to this file" ]
  += Arg.help "Train a Markov chain from standard input"

run = Arg.record Run{chain=undefined}
  [ chain := error "Must specify input chain"
          += Arg.typFile
          += Arg.help "Read chain from this file" ]
  -- += Arg.help "Generate text from a Markov chain, forever"

modes  = Arg.modes_  [run,train]
      += Arg.program "andreyevich"
      += Arg.summary "andreyevich: Markov chain text generator"
      -- += Arg.help    "Build and run Markov chains for text generation"

mode :: Mode -> IO ()

mode Train{num,out}
  | num < 0 = error "train: n must be at least 0"
  | otherwise = Txt.getContents
    >>= BSL.writeFile out . Z.compress . Bin.encode . makeChain num

mode Run{chain} = (Z.decompress <$> BSL.readFile chain)
  >>= RNG.withSystemRandom . runChain . Bin.decode

main :: IO ()
main = Arg.cmdArgs_ modes >>= mode
