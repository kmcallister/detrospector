{-# LANGUAGE
    BangPatterns
  , ViewPatterns
  , PatternGuards
  , DeriveDataTypeable #-}
module Main(main) where

import Data.List
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
import           System.Console.CmdArgs((&=))
import qualified Data.Serialize         as Cer
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Codec.Compression.GZip as Z

-- table recording character frequency,
-- Char mapped to Int
type FreqTable = IM.IntMap Int

sing :: Char -> FreqTable
sing x = IM.singleton (fromEnum x) 1

incr :: Char -> FreqTable -> FreqTable
incr x = IM.alter (Just . f) $ fromEnum x where
  f Nothing  = 1
  f (Just n) = n+1
  
-- to pick from (PickTable n im):
-- pick random k from [0,n), then index im at
-- first key > k
data PickTable = PickTable Int (IM.IntMap Char)
  deriving (Show)

cumulate :: FreqTable -> PickTable
cumulate t = PickTable r $ IM.fromList ps where
  (r,ps) = mapAccumL f 0 $ IM.assocs t
  f ra (x,n) = let rb = ra+n in (rb, (rb, toEnum x))

type Queue a = S.Seq a

shift :: Int -> a -> Queue a -> Queue a
shift n x q
  | S.length q < n          = q S.|> x
  | (_ S.:< s) <- S.viewl q = s S.|> x
  | otherwise               = q S.|> x

-- (Chain n p hm) maps n-char subsequences to PickTables
data Chain = Chain Int PickTable (H.HashMap (Queue Char) PickTable)
  deriving (Show)

-- orphan
instance (H.Hashable a) => H.Hashable (S.Seq a) where
  {-# SPECIALIZE instance H.Hashable (S.Seq Char) #-}
  hash = F.foldl' (\acc h -> acc `H.combine` H.hash h) 0

makeChain :: Int -> String -> Chain
makeChain n ys = Chain n (cumulate $ prior ys) hm where
  hm = H.map cumulate $ go S.empty H.empty ys
  go _ h [] = h
  go !s !h (x:xs)
    | S.length s < n = go (s S.|> x) h xs
    | otherwise      = go (shift n x s) (ins s x h) xs
  ins s x h
    | s `H.member` h = H.adjust (incr x) s h
    | otherwise      = H.insert s (sing x) h
  prior = foldl' (flip incr) IM.empty

type RNG = RNG.GenIO

intMod :: RNG -> Int -> IO Int
intMod g n = (`mod` n) <$> RNG.uniform g

pick :: RNG -> PickTable -> IO Char
pick g (PickTable n im) = do
  k <- intMod g n
  case IM.split k im of
    -- the last key in im is n, so the right list cannot be empty
    (_, IM.toList -> ((_,x):_)) -> return x
    _ -> error "impossible"

runChain :: RNG -> Chain -> IO ()
runChain g (Chain n pri h) = go S.empty where
  tbl s
    | Just t <- H.lookup s h = t
    | otherwise = pri
  go !s = do
    x <- pick g $ tbl s
    putChar x
    go (shift n x s)

-- orphan
instance (Cer.Serialize k, Cer.Serialize v, H.Hashable k, Ord k)
       => Cer.Serialize (H.HashMap k v) where
  put = Cer.put . H.assocs
  get = H.fromList <$> Cer.get

instance Cer.Serialize PickTable where
  put (PickTable n t) = Cer.put (n,t)
  get = (\(n,t) -> PickTable n t) <$> Cer.get

instance Cer.Serialize Chain where
  put (Chain n pri h) = Cer.put (n,pri,h)
  get = (\(n,pri,h) -> Chain n pri h) <$> Cer.get

data Markov
  = Train   { num   :: Int,
              out   :: FilePath }
  | Run     { chain :: FilePath }
  deriving (Show, Data, Typeable)

train, run :: Markov
train = Train { num    = Arg.def
                      &= Arg.help "Number of characters lookback",
                out    = Arg.def
                      &= Arg.typFile
                      &= Arg.help "Write chain to this file" }
run   = Run   { chain  = Arg.def
                      &= Arg.typFile
                      &= Arg.help "Read chain from this file" }

mode :: Markov -> IO ()
mode (Train n _) | n < 1 = error "train: n must be at least 1"
mode (Train n o) = do
  c <- getContents
  let bs = Cer.encode $ makeChain n c
  BSL.writeFile o . Z.compress . BSL.fromChunks $ [bs]
mode (Run c) = do
  cf <- (BS.concat . BSL.toChunks . Z.decompress) <$> BSL.readFile c
  case Cer.decode cf of
    Left  err -> error ("parse error: " ++ err)
    Right ch  -> RNG.withSystemRandom $ \g -> runChain g ch

main :: IO ()
main = Arg.cmdArgs (Arg.modes [train, run]) >>= mode
