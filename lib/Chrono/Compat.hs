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
-- | Compatibility with types in the built-in **time** package.

module Chrono.Compat
(
    convertToDiffTime
) where

import Data.Time.Clock

import Chrono.TimeStamp

--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'NominalDiffTime', allowing you to then use the time manipulation
-- functions in "Data.Time.Clock"
--
convertToDiffTime :: TimeStamp -> NominalDiffTime
convertToDiffTime = fromRational . (/ 1e9) . fromIntegral
