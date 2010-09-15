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

-- table recording character frequency,
-- Char mapped to Int
type FreqTable = IM.IntMap Int

-- to pick from (PickTable n im):
-- pick random k from [0,n), then index im at
-- first key > k
data PickTable = PickTable Int (IM.IntMap Char)
  deriving (Show)

cumulate :: FreqTable -> PickTable
cumulate t = PickTable r $ IM.fromList ps where
  (r,ps) = mapAccumR f 0 $ IM.assocs t
  f ra (x,n) = let rb = ra+n in (rb, (rb, toEnum x))

type Queue a = S.Seq a

shift :: Int -> a -> Queue a -> Queue a
shift n x q
  | S.length q < n          = q S.|> x
  | (_ S.:< s) <- S.viewl q = s S.|> x
  | otherwise               = q S.|> x

-- (Chain n p hm) maps n-char subsequences to PickTables
-- p is the 'prior' distribution over the whole input
data Chain = Chain Int PickTable (H.HashMap (Queue Char) PickTable)
  deriving (Show)

-- orphan
instance (H.Hashable a) => H.Hashable (S.Seq a) where
  {-# SPECIALIZE instance H.Hashable (S.Seq Char) #-}
  hash = F.foldl' (\acc h -> acc `H.combine` H.hash h) 0

makeChain :: Int -> Txt.Text -> Chain
makeChain n ys = Chain n (cumulate $ prior ys) hm where
  hm = H.map cumulate . snd $ Txt.foldl' roll (S.empty,H.empty) ys

  roll (!s,!h) x
    | S.length s < n = (s S.|> x , h)
    | otherwise      = (shift n x s, H.alter (ins x) s h)

  ins x Nothing  = Just $! sing x
  ins x (Just v) = Just $! incr x v

  prior = Txt.foldl' (flip incr) IM.empty

  sing x = IM.singleton (fromEnum x) 1

  incr x = IM.alter f $ fromEnum x where
    f Nothing  = Just 1
    f (Just v) = Just $! (v+1)

runChain :: Chain -> RNG.GenIO -> IO ()
runChain (Chain n pri h) g = go S.empty where
  go s = do
    x <- pick . fromMaybe pri $ H.lookup s h
    putChar x
    go $! shift n x s

  pick (PickTable t im) = do
    k <- (`mod` t) <$> RNG.uniform g
    case IM.split k im of
      -- last key in im is t, and we know k < t
      -- therefore the right list cannot be empty
      (_, IM.toList -> ((_,x):_)) -> return x
      _ -> error "impossible"

-- orphan
instance (Bin.Binary k, Bin.Binary v, H.Hashable k, Ord k)
       => Bin.Binary (H.HashMap k v) where
  put = Bin.put . H.assocs
  get = H.fromList <$> Bin.get

instance Bin.Binary PickTable where
  put (PickTable n t) = Bin.put (n,t)
  get = uncurry PickTable <$> Bin.get

instance Bin.Binary Chain where
  put (Chain n pri h) = Bin.put (n,pri,h)
  get = (\(n,pri,h) -> Chain n pri h) <$> Bin.get

data Andreyevich
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

mode :: Andreyevich -> IO ()

mode Train{num,out}
  | num < 0 = error "train: n must be at least 0"
  | otherwise = Txt.getContents
    >>= BSL.writeFile out . Z.compress . Bin.encode . makeChain num

mode Run{chain} = (Z.decompress <$> BSL.readFile chain)
  >>= RNG.withSystemRandom . runChain . Bin.decode

main :: IO ()
main = Arg.cmdArgs_ modes >>= mode
