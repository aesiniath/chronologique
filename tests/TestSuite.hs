{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import CheckTimeStamp
import CheckVectorOperations

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    checkTimeStamp
    checkVectorUnboxed
