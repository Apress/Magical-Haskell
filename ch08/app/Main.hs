{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.List (sortOn)
import qualified Data.Text as T
import System.Random
import Control.Monad (when, unless, forM)
import Text.Read (readMaybe)

-- Let's start with our basic types

main = putStrLn "Hello World"