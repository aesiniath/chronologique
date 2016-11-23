{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import CheckTimeStamp

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    checkTimeStamp

