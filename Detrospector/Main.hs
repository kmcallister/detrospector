{-# OPTIONS_GHC
    -fno-warn-missing-fields #-}
module Detrospector.Main(main) where

import Detrospector.Modes

import qualified Detrospector.Modes.Train  as M
import qualified Detrospector.Modes.Run    as M
import qualified Detrospector.Modes.Neolog as M

import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)))

modes :: Annotate Arg.Ann
modes  = Arg.modes_  [train,run,neolog]
      += Arg.program "detrospector"
      += Arg.summary "detrospector: Markov chain text generator"
      += Arg.help    "Build and run Markov chains for text generation"
  where

  train = Arg.record Train{}
    [ num := 4
          += Arg.help "Number of characters lookback"
    , out := error "Must specify output chain"
          += Arg.typFile
          += Arg.help "Write chain to this file" ]
    += Arg.help "Train a Markov chain from standard input"
  
  run = Arg.record Run{}
    [ chain := error "Must specify input chain"
            += Arg.argPos 0
            += Arg.typ "CHAIN_FILE" ]
    += Arg.help "Generate random text"
  
  neolog = Arg.record Neolog{}
    [ chain    := error "Must specify input chain"
               += Arg.argPos 0
               += Arg.typ "CHAIN_FILE"
    , wordFile := "/usr/share/dict/words" --FIXME: platform?
               += Arg.typFile
               += Arg.explicit += Arg.name "w" += Arg.name "words"
               += Arg.help "List of real words, to be ignored"
    , minLen   := 5
               += Arg.explicit += Arg.name "m" += Arg.name "min"
               += Arg.help "Minimum length of a word"
    , maxLen   := 20
               += Arg.explicit += Arg.name "n" += Arg.name "max"
               += Arg.help "Maximum length of a word" ]
    += Arg.help "Invent new words not found in a dictionary"

dispatch :: Mode -> IO ()
dispatch m = case m of
  Train {} -> M.train  m
  Run   {} -> M.run    m
  Neolog{} -> M.neolog m

main :: IO ()
main = Arg.cmdArgs_ modes >>= dispatch
