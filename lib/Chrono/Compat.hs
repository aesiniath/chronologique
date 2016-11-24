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

--
-- | Compatibility with time types from other time handling libraries.

module Chrono.Compat
(
    convertToBaseTime
  , convertToHourglass
) where

import Data.Time.Clock.POSIX
import Data.Hourglass

import Chrono.TimeStamp

--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'NominalDiffTime', allowing you to then use the time manipulation
-- functions in "Data.Time.Clock" from __base__.
--
convertToBaseTime :: TimeStamp -> POSIXTime
convertToBaseTime = fromRational . (/ 1e9) . fromIntegral

--
--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'ElapsedP', allowing you to then use the time manipulation
-- functions in the __hourglass__ package.
--
convertToHourglass :: TimeStamp -> ElapsedP
convertToHourglass = timeGetElapsedP
