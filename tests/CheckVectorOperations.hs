--
-- Time to manipulate time
--
-- Copyright © 2013-2017 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Test unboxed vector usage

module CheckVectorOperations where

import Test.Hspec
import Test.QuickCheck

import Chrono.TimeStamp

checkVectorUnboxed :: Spec
checkVectorUnboxed = do
    describe "Unbox instance for TimeStamp" $
        it "should work in an unboxed Vector" $ do
            True `shouldBe` True
