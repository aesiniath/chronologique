--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Vaultaire.Types.TimeStamp
(
    TimeStamp(..),
    convertToDiffTime,
    convertToTimeStamp,
    getCurrentTimeNanoseconds
) where

import Control.Applicative
import Data.Maybe
import Data.Packer (getWord64LE, putWord64LE, runPacking, tryUnpacking)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Word (Word64)
import System.Locale
import Test.QuickCheck

import Vaultaire.Classes.WireFormat

--
-- | Number of nanoseconds since the Unix epoch, stored in a Word64.
--
-- The Show instance displays the TimeStamp as seconds with the nanosecond
-- precision expressed as a decimal amount after the interger, ie:
--
-- >>> t <- getCurrentTimeNanoseconds
-- >>> show t
-- 2014-07-31T23:09:35.274387000Z
--
-- However this doesn't change the fact the underlying representation counts
-- nanoseconds since epoch:
--
-- >>> show $ unTimeStamp t
-- 1406848175274387000
--
-- There is a Read instance that is reasonably accommodating.
--
-- >>> read "2014-07-31T13:05:04.942089001Z" ::TimeStamp
-- 2014-07-31T13:05:04.942089001Z
--
-- >>> read "1406811904.942089001" :: TimeStamp
-- 2014-07-31T13:05:04.942089001Z
--
-- >>> read "1406811904" :: TimeStamp
-- 2014-07-31T13:05:04.000000000Z
--
newtype TimeStamp = TimeStamp {
    unTimeStamp :: Word64
} deriving (Eq, Ord, Enum, Arbitrary, Num, Real, Integral, Bounded)

instance Show TimeStamp where
    show (TimeStamp t) =
      let
        seconds = posixSecondsToUTCTime $ realToFrac (fromIntegral t / 1000000000 :: Rational)
        iso8601 = formatTime defaultTimeLocale "%FT%T.%q" seconds
      in
        -- trim to nanoseconds
        take 29 iso8601 ++ "Z"

instance Read TimeStamp where
    readsPrec _ s = maybeToList $ (,"") <$> convertToTimeStamp <$> parse s
      where
        parse :: String -> Maybe UTCTime
        parse x =   parseTime defaultTimeLocale "%FT%T%Q%Z" x
                <|> parseTime defaultTimeLocale "%F" x
                <|> parseTime defaultTimeLocale "%s%Q" x

instance WireFormat TimeStamp where
    toWire = runPacking 8 . putWord64LE . unTimeStamp
    fromWire = tryUnpacking (TimeStamp `fmap` getWord64LE)

--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'NominalDiffTime', allowing you to then use the time manipulation
-- functions in "Data.Time.Clock"
--
convertToDiffTime :: TimeStamp -> NominalDiffTime
convertToDiffTime = fromRational . (/ 1e9) . fromIntegral

--
-- | Get the current system time, expressed as a 'TimeStamp' (which is to
-- say, number of nanoseconds since the Unix epoch).
--
{-
    getPOSIXTime returns a NominalDiffTime with picosecond precision. So
    convert it to nanoseconds, and discard any remaining fractional amount.
-}
getCurrentTimeNanoseconds :: IO TimeStamp -- Word64
getCurrentTimeNanoseconds = do
    u <- getCurrentTime
    return $ convertToTimeStamp u

{-
    This code adapted from the implementation in Data.Time.Clock.POSIX. The
    time types in base are hopeless. Julian days? Really? We'll replace this
    with hs-hourglass shortly.
-}

secondsPerDay :: Integer
secondsPerDay = 86400

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587

convertToTimeStamp :: UTCTime -> TimeStamp
convertToTimeStamp (UTCTime day secs) =
  let
    mark = diffDays day unixEpochDay * secondsPerDay * 1000000000
    nano = floor $ (*1000000000) $ toRational secs
  in
    TimeStamp $ fromIntegral $ mark + nano
