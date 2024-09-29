{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import GameFloor

main = execStateT gameCycle initialState