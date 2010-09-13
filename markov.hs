{-# LANGUAGE
    BangPatterns
  , ViewPatterns
  , PatternGuards
  , DeriveDataTypeable #-}
module Main(main) where

import qualified Data.HashMap      as H
import qualified Data.Hashable     as H
import qualified Data.IntMap       as IM
import qualified System.Random.MWC as RNG
import qualified Data.Sequence     as S
import qualified Data.Foldable     as F
import Data.List
import Control.Applicative
import System.Console.CmdArgs

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
  deriving (Show, Read)

cumulate :: FreqTable -> PickTable
cumulate t = PickTable r $ IM.fromList ps where
  (r,ps) = mapAccumL f 0 $ IM.assocs t
  f r (x,n) = let r' = r+n in (r', (r', toEnum x))

type Queue a = S.Seq a

shift :: Int -> a -> Queue a -> Queue a
shift n x q
  | S.length q < n          = q S.|> x
  | (_ S.:< s) <- S.viewl q = s S.|> x
  | otherwise               = q S.|> x

-- (Chain n p hm) maps n-char subsequences to PickTables
data Chain = Chain Int PickTable (H.HashMap (Queue Char) PickTable)
  deriving (Show, Read)

-- orphan :/
instance (H.Hashable a) => H.Hashable (S.Seq a) where
  {-# SPECIALIZE instance H.Hashable (S.Seq Char) #-}
  hash = F.foldl' (\acc h -> acc `H.combine` H.hash h) 0

makeChain :: Int -> String -> Chain
makeChain n xs = Chain n (cumulate $ prior xs) h where
  h = H.map cumulate $ go S.empty H.empty xs
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
    (_, IM.toList -> ((_,x):_)) -> return x

runChain :: RNG -> Chain -> IO ()
runChain g (Chain n pri h) = go S.empty where
  tbl s
    | Just t <- H.lookup s h = t
    | otherwise = pri
  go !s = do
    x <- pick g $ tbl s
    putChar x
    go (shift n x s)

data Markov
  = Train   { n     :: Int,
              out   :: FilePath }
  | Run     { chain :: FilePath }
  deriving (Show, Data, Typeable)

train, run :: Markov
train = Train { n     = def
                     &= help "Number of characters lookback",
                out   = def
                     &= typFile
                     &= help "Write chain to this file" }
run   = Run   { chain
                      = def
                     &= typFile
                     &= help "Read chain from this file" }

go :: Markov -> IO ()
go (Train n _) | n < 1 = error "train: n must be at least 1"
go (Train n o) = do
  c <- getContents
  writeFile o . show $ makeChain n c
go (Run c) = do
  cf <- readFile c
  ch <- readIO cf
  RNG.withSystemRandom $ \g -> runChain g ch

main :: IO ()
main = cmdArgs (modes [train, run]) >>= go
