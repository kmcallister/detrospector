{-# LANGUAGE
    DeriveDataTypeable #-}
module Detrospector.Modes(Mode(..), ModeFun) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

data Mode
  = Train   { num      :: Int
            , out      :: FilePath }
  | Run     { chain    :: FilePath }
  | Neolog  { chain    :: FilePath
            , minLen   :: Int
            , maxLen   :: Int
            , wordFile :: FilePath }
  deriving (Show, Typeable, Data)

type ModeFun = Mode -> IO ()
