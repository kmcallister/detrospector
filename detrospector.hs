{-# LANGUAGE
    BangPatterns
  , ViewPatterns
  , PatternGuards
  , DeriveDataTypeable
  , NamedFieldPuns
  , RecordWildCards #-}
module Main(main) where

import Data.Char
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Typeable(Typeable)
import Data.Data    (Data    )
import qualified Data.HashMap           as H
import qualified Data.HashSet           as HS
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

-- Sample from a PickTable.
sample :: PickTable -> RNG.GenIO -> IO Char
sample (PickTable t im) g = do
  k <- (`mod` t) <$> RNG.uniform g
  case IM.split k im of
    -- last key in im is t, and we know k < t
    -- therefore the right list cannot be empty
    (_, IM.toList -> ((_,x):_)) -> return x
    _ -> error "impossible"

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

-- Pick from a chain according to history.  Returns a character
-- and the amount of history actually used.
pick :: Chain -> Queue Char -> RNG.GenIO -> IO (Char, Int)
pick (Chain _ h) s g = do x <- sample pt g; return (x,hn) where
  (pt, hn) = get s

  -- assumption: map is nonempty for empty key
  get  t = fromMaybe (get $ qTail t) $ look t where
    qTail (S.viewl -> (_ S.:< r)) = r
    qTail _ = error "qTail: empty queue"
  look t = do x <- H.lookup t h; return (x, S.length t)

-- Run a Markov chain, printing output forever.
runChain :: Chain -> RNG.GenIO -> IO ()
runChain c@(Chain n _) g = go S.empty where
  go s = do
    (x,_) <- pick c s g
    putChar x
    go $! shift n x s

-- Generate neologisms.
neolog :: Mode -> Chain -> RNG.GenIO -> IO ()
neolog Neolog{minLen,maxLen,wordFile} c@(Chain n _) g
  = getWords >>= go where

  getWords = (HS.fromList . map (S.fromList . Txt.unpack) . Txt.lines)
    <$> Txt.readFile wordFile

  go !wd = do
    xs  <- fmap toLower <$> make S.empty S.empty
    wdd <- if (S.length xs >= minLen) && HS.notMember xs wd
      then putStrLn (F.toList xs) >> return (HS.insert xs wd)
      else return wd
    go wdd

  make !xs !s | S.length s >= maxLen = return s
              | otherwise = do
      (x,h) <- pick c s g
      -- give up if we don't use all history
      if (h == S.length s) && isAlpha x
        then make (xs S.|> x) (shift n x s)
        else return xs

neolog _ _ _ = error "impossible: wrong mode passed to neolog"

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

withChain :: FilePath -> (Chain -> RNG.GenIO -> IO a) -> IO a
withChain p f = do
  ch <- (Bin.decode . Z.decompress) <$> BSL.readFile p
  RNG.withSystemRandom $ f ch

data Mode
  = Train   { num      :: Int
            , out      :: FilePath }
  | Run     { chain    :: FilePath }
  | Neolog  { chain    :: FilePath
            , minLen   :: Int
            , maxLen   :: Int
            , wordFile :: FilePath }
  deriving (Show, Typeable, Data)

m_train, m_run, m_neolog, modes :: Annotate Arg.Ann

m_train = Arg.record Train{}
  [ num := 3
        += Arg.help "Number of characters lookback"
  , out := error "Must specify output chain"
        += Arg.typFile
        += Arg.help "Write chain to this file" ]
  += Arg.help "Train a Markov chain from standard input"

m_run = Arg.record Run{}
  [ chain := error "Must specify input chain"
          += Arg.argPos 0
          += Arg.typ "CHAIN_FILE" ]
  -- += Arg.help "Generate random text"

m_neolog = Arg.record Neolog{}
  [ chain    := error "Must specify input chain"
             += Arg.argPos 0
             += Arg.typ "CHAIN_FILE"
  , wordFile := "/usr/share/dict/words" --FIXME: platform?
             += Arg.typFile
             += Arg.help "List of real words, to be ignored"
  , minLen   := 5
             += Arg.explicit += Arg.name "m" += Arg.name "minLen"
             += Arg.help "Minimum length of a word"
  , maxLen   := 20
             += Arg.explicit += Arg.name "n" += Arg.name "maxLen"
             += Arg.help "Maximum length of a word" ]
  += Arg.help "Generate random words"

modes  = Arg.modes_  [m_run,m_train,m_neolog]
      += Arg.program "detrospector"
      += Arg.summary "detrospector: Markov chain text generator"
      -- += Arg.help    "Build and run Markov chains for text generation"

mode :: Mode -> IO ()

mode Train{..}
  | num < 0 = error "train: n must be at least 0"
  | otherwise = Txt.getContents
    >>= BSL.writeFile out . Z.compress . Bin.encode . makeChain num

mode Run{chain}
  = withChain chain runChain

mode m@Neolog{chain}
  = withChain chain $ neolog m

main :: IO ()
main = Arg.cmdArgs_ modes >>= mode
