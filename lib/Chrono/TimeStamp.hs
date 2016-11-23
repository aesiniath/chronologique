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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Chrono.TimeStamp
(
    TimeStamp(..),
    convertToDiffTime,
    convertToTimeStamp,
    getCurrentTimeNanoseconds,

    ISO8601_Precise(..)
) where

import Control.Applicative
import Data.Maybe
import Data.Time.Clock
import Data.Int (Int64)
import Data.Word (Word64)
import Data.Hourglass
import Time.System

import Chrono.Formats

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
-- >>> read "2014-07-31T13:05:04.942089001Z" :: TimeStamp
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
} deriving (Eq, Ord, Enum, Num, Real, Integral, Bounded)

instance Timeable TimeStamp where
    timeGetElapsedP :: TimeStamp -> ElapsedP
    timeGetElapsedP (TimeStamp ticks) =
      let
        (s,ns) = divMod ticks 1000000000

        cast :: Word64 -> Int64
        cast = fromInteger . toInteger
      in
        ElapsedP (Elapsed (Seconds (cast s))) (NanoSeconds (cast ns))

instance Show TimeStamp where
    show t =
        timePrint ISO8601_Precise t

instance Read TimeStamp where
    readsPrec _ s = maybeToList $ (,"") <$> reduceToTimeStamp <$> parse s
      where
        parse :: String -> Maybe DateTime
        parse x =   timeParse ISO8601_Precise x
                <|> timeParse ISO8601_Seconds x
                <|> timeParse ISO8601_Days x
                <|> timeParse ISO8601_DateAndTime x -- from hourglass
                <|> timeParse Posix_Precise x
                <|> timeParse Posix_Seconds x

reduceToTimeStamp :: DateTime -> TimeStamp
reduceToTimeStamp = convertToTimeStamp . timeGetElapsedP

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
    p <- timeCurrentP
    return $ convertToTimeStamp p

convertToTimeStamp :: ElapsedP -> TimeStamp
convertToTimeStamp (ElapsedP (Elapsed (Seconds seconds)) (NanoSeconds nanoseconds)) =
  let
    s  = fromIntegral seconds :: Word64
    ns = fromIntegral nanoseconds
  in
    TimeStamp (s * 1000000000) + ns

