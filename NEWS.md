# User-visible changes in the pbapply R package

## Version 1.3-1, October 5, 2016

* `timerProgressBar` gained 2 new styles with flexible styling including
  left/right end and elapsed/remaining components of the progress bar.
* `timerProgressBar` gained `min_time` argument for minimum processing time
  (in seconds) that is required to show a progress bar.

## Version 1.3-0, September 25, 2016

* Progress bar is added for parallel (cluster and forking) jobs (request #9 from Kendon Bell).
  Package parallel is now imported as a result.
* `timerProgressBar` prints days when job is expected to be >24h (PR #11 from Phil Chalmers).

## Version 1.2-2, August 25, 2016

* Stylistic changes in printed messages.
* Default pb type is `'none'` when `!interactive()`.

## Version 1.2-1, March 2, 2016

* Double tilde `~` in timerProgressBar cleaned up (`~~calculating`).
* `timerProgressBar` gained 4 styles as documented on the help page.
  Showing elapsed and remaining time, throbber and bar formats.

## Version 1.2-0, Feb 29, 2016

* New function: `timerProgressBar` written by Zygmunt Zawadzki.
* Zygmunt added as package author.

## Version 1.1-3, Nov 24, 2015

* R (>= 3.2.0) dependency added because check
  failed on R-oldrelease (R-3.1.3) with error
  `'could not find function "forceAndCall"'`
  that was part of the `apply` function source code.
  Reported by Uwe Ligges.

## Version 1.1-2, Nov 21, 2015

* Using on.exit and invisible(`NULL`) in examples.
* `pblapply` did not return `NULL` values, reported by
  J. Barrett, now fixed.
* `pblapply` did not return list names, now fixed.
* `pbapply`, `pbsapply`, `pbreplicate`: code follows base original.
* Examples updated to follow base R examples.
* Rd file updated to to match code changes.

## Version 1.1-1, Feb 3, 2014

* pblapply did not pass `...` when not in interactive mode.
  Bug reported by R. D. Morey (U Groningen).

## Version 1.1-0, Sept 25, 2013

* Removed `:::` to satisfy R 3.0.2 checks.

## Version 1.0-5, July 6, 2012

* `inst/COPYING` removed.
* `.Internal` call removed from `pblapply`.

## Version 1.0-4, September 8, 2011

* `.onLoad` added to `zzz.R`
* Help files a bit reworked.

## Version 1.0-3, September 9, 2010

* `pboptions.Rd` modified: pb type values added.

## Version 1.0-2, September 4, 2010

* `pboptions` function reworked.
* Functions simplified.

## Version 1.0-1, September 3, 2010

* `pbreplicate` added.
* `/tests` directory created.
* `R CMD check` failed on unix systems:
  `/man` and `/R` directories reworked.

## Version 1.0-0, September 2, 2010

* First release.
