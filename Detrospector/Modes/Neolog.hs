{-# LANGUAGE
    NamedFieldPuns
  , BangPatterns #-}
module Detrospector.Modes.Neolog(neolog) where

import Detrospector.Types
import Detrospector.Modes

import Data.Char
import Control.Applicative
import qualified Data.HashSet      as HS
import qualified Data.Text.Lazy    as Txt
import qualified Data.Text.Lazy.IO as Txt

-- Generate neologisms.
go :: Mode -> Chain -> RNG -> IO ()
go Neolog{minLen,maxLen,wordFile} c@(Chain n _) g
  = getWords >>= loop where

  getWords = (HS.fromList . map (listToQueue . Txt.unpack) . Txt.lines)
    <$> Txt.readFile wordFile

  loop !wd = do
    xs  <- fmap toLower <$> make emptyQ emptyQ
    wdd <- if (qLength xs >= minLen) && not (HS.member xs wd)
      then putStrLn (queueToList xs) >> return (HS.insert xs wd)
      else return wd
    loop wdd

  make !xs !s | qLength xs >= maxLen = return xs
              | otherwise = do
      (x,h) <- pick c s g
      -- give up if we don't use all history
      if (h == qLength s) && isAlpha x
        then make (xs `qSnoc` x) (shift n x s)
        else return xs

go _ _ _ = error "impossible: wrong mode passed to neolog"

neolog :: ModeFun
neolog m@Neolog{chain} = withChain chain $ go m
neolog _ = error "impossible: wrong mode passed to neolog"
