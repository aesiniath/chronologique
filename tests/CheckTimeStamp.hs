--
-- Time to manipulate time
--
-- Copyright © 2013-2016 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Test serialisation/deserialiastion for TimeStamp type

module CheckTimeStamp where

import Test.Hspec
import Data.Hourglass

import Chrono.TimeStamp

checkTimeStamp :: Spec
checkTimeStamp = do
  describe "Adaptor around Hourglass types" $ do
        it "formats a known date correctly" $ do
            timePrint ISO8601_Precise (TimeStamp 1406849015948797001) `shouldBe` "2014-07-31T23:23:35.948797001Z"

        it "uses timeParse effectively" $ do
            timeParse ISO8601_Precise "2014-07-31T23:42:35.948797001Z" `shouldBe`
                Just (DateTime (Date 2014 July 31) (TimeOfDay 23 42 35 948797001))

  describe "Round trip through Read and Show instances" $ do
    it "outputs a correctly formated ISO 8601 timestamp when Shown" $ do
      show (TimeStamp 1406849015948797001) `shouldBe` "2014-07-31T23:23:35.948797001Z"
      show (TimeStamp 1406849015948797001) `shouldBe` "2014-07-31T23:23:35.948797001Z"
      show (TimeStamp 0) `shouldBe` "1970-01-01T00:00:00.000000000Z"

    it "Reads ISO 8601 timestamps" $ do
      read "2014-07-31T23:23:35.948797001Z" `shouldBe` TimeStamp 1406849015948797001
      read "2014-07-31T23:23:35Z" `shouldBe` TimeStamp 1406849015000000000
      read "2014-07-31" `shouldBe` TimeStamp 1406764800000000000

    it "reads the Unix epoch date" $
      read "1970-01-01" `shouldBe` TimeStamp 0

    it "permissively reads various formats" $ do
      show (read "1970-01-01T00:00:00.000000000Z" :: TimeStamp) `shouldBe` "1970-01-01T00:00:00.000000000Z"
      show (read "1970-01-01" :: TimeStamp) `shouldBe` "1970-01-01T00:00:00.000000000Z"
      show (read "0" :: TimeStamp) `shouldBe` "1970-01-01T00:00:00.000000000Z"

