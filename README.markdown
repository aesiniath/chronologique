Making time to manipulate time
==============================

The standard package for working with dates and times in Haskell, **time**, is
_awkward_. That's a subjective judgment, but over the years there have been few
areas more frustrating than trying to do pragmatic things with calendars and
clocks.

This package represents some opinionated approaches to working with times and
dates. It also is just a place to collect some hard-won idioms for converting
between things.

You absolutely do not need to use this; if you can make sense of the basic time
and calendar types available by default then by all means have at it.

This package was seeded using the TimeStamp type originally from the Vaultaire
project, a time-series database for systems metrics. That type was originally
implemented as a shim on top of base's **time** package; limitations there have
led it to be reimplemented atop of the very complete (but also somewhat
complicated) **hourglass** library.

Our original use was wanting to conveniently measure things happening on
distributed computer systems. Since machine clock cycles are in units of
nanoseconds, this has the nice property that, assuming the system clock is not
corrupted, two subsequent events from the same source process are guaranteed to
have monotonically increasing timestamps. And even if the system clock goes to
hell, they're still decently likely to be unique per device. Make for good
keys.

So the TimeStamp type herein is nanoseconds since the Unix epoch; which in 64
bits means that you can represent times between early in the morning of 21
September 1677 through just before midnight on 11 April 2262. The primary use
isn't doing calendaring, though; it's just working with machine generated
timestamps in distributed systems.

