# Version 1.7-2, June 27, 2023

* Documented `.pb_env` environment to address WARNING after CRAN submission.

# Version 1.7-1, June 26, 2023

* Future backend not working on Windows (#63), implemented fix similar to PR #64.

# Version 1.7-0, Jan 12, 2023

* New functions: `pbeapply`, `pbvapply`, `pbby`, `pbMap` (#50, #51, #52, #53).
* Added support for future backends (#54), and the future and future.apply packages are now Suggested.
* Henrik Bengtsson (@HenrikBengtsson) and R Core Team added as contributors.
* `NEWS.md` file is not excluded from package tarball (#58).
* `pbapply` performance issue (#62) addressed.
* License for the package is now GPL (>=2) (#61).

# Version 1.6-0, Nov 13, 2022

* New function `pbwalk` that can be called for side-effects (#48).

# Version 1.5-0, Sept 15, 2021

* New progress bar type `"shiny"` to show the progress bar in the Shiny UI.
* Following R changes to `apply`, fixing dimnames issue (#44).

# Version 1.4-3, August 11, 2020

* Following R 3.6.2 changes to `apply` (#41).
* Adding `pbtapply` (#21).

# Version 1.4-2, August 30, 2019

* Fixed environment issue inside the tracer in `pbmapply`
  in non-interactive session (issue #39).
  This came up in a package suggesting pbapply, reported by @Nowosad.

# Version 1.4-1, July 14, 2019

* Use `base::strrep` instead of `paste(rep(), collapse='')` in `timerProgressBar`.
* Fixed space printing glitch (due to rounding), causing issue #37.

# Version 1.4-0, February 5, 2019

* `pblapply` returns empty list for empty vector consistent with
  `lapply` behavior (#33 by @kendonB); tests also added.
* `pbmapply` function added (#29) without parallel option.

# Version 1.3-4, January 9, 2018

* New function `pbtypes()` to print available pb types depending on OS.
* `getTimeAsString` is now exported, with description and examples.
* New `pboption` `use_lb` to switch on load balancing for parallel clusters,
  `FALSE` by default (feature request #28).

# Version 1.3-3, July 3, 2017

* Bar did not show up at start. This could happen if
  it was waiting to calculate total time to compare it
  to `min_time` option. Bar is shown right away if  `min_time=0`
  (issue #18).
* `pbapply` gained `cl` argument to be consistent with
  other parallelized functions with progress bar (issue #24).

# Version 1.3-2, February 28, 2017

* `timerProgressBar` makes sure `char` argument is not empty
  (`""` is replaced by the default `"="`).
* Progress bar is only shown when
  `interactive() && is.null(getOption("knitr.in.progress"))`
  to avoid printing progress bar during interactive knitr rendering
  (request #15 from Sergio Oller).
* Bugfix: functions failed with single cluster `cl` argument (issue #17).

# Version 1.3-1, October 30, 2016

* `timerProgressBar` gained 2 new styles with flexible styling including
  left/right end and elapsed/remaining components of the progress bar.
* `timerProgressBar` gained `min_time` argument for minimum processing time
  (in seconds) that is required to show a progress bar.
  The global `min_time` can be set via `pboptions`.

# Version 1.3-0, September 25, 2016

* Progress bar is added for parallel (cluster and forking) jobs
(request #9 from Kendon Bell).
  Package parallel is now imported as a result.
* `timerProgressBar` prints days when job is expected to be >24h
(PR #11 from Phil Chalmers).

# Version 1.2-2, August 25, 2016

* Stylistic changes in printed messages.
* Default pb type is `'none'` when `!interactive()`.

# Version 1.2-1, March 2, 2016

* Double tilde `~` in `timerProgressBar` cleaned up (`~~calculating`).
* `timerProgressBar` gained 4 styles as documented on the help page.
  Showing elapsed and remaining time, throbber and bar formats.

# Version 1.2-0, Feb 29, 2016

* New function: `timerProgressBar` written by Zygmunt Zawadzki.
* Zygmunt added as package author.

# Version 1.1-3, Nov 24, 2015

* R (>= 3.2.0) dependency added because check
  failed on R-oldrelease (R-3.1.3) with error
  `'could not find function "forceAndCall"'`
  that was part of the `apply` function source code.
  Reported by Uwe Ligges.

# Version 1.1-2, Nov 21, 2015

* Using on.exit and invisible(`NULL`) in examples.
* `pblapply` did not return `NULL` values, reported by
  J. Barrett, now fixed.
* `pblapply` did not return list names, now fixed.
* `pbapply`, `pbsapply`, `pbreplicate`: code follows base original.
* Examples updated to follow base R examples.
* Rd file updated to to match code changes.

# Version 1.1-1, Feb 3, 2014

* pblapply did not pass `...` when not in interactive mode.
  Bug reported by R. D. Morey (U Groningen).

# Version 1.1-0, Sept 25, 2013

* Removed `:::` to satisfy R 3.0.2 checks.

# Version 1.0-5, July 6, 2012

* `inst/COPYING` removed.
* `.Internal` call removed from `pblapply`.

# Version 1.0-4, September 8, 2011

* `.onLoad` added to `zzz.R`
* Help files a bit reworked.

# Version 1.0-3, September 9, 2010

* `pboptions.Rd` modified: pb type values added.

# Version 1.0-2, September 4, 2010

* `pboptions` function reworked.
* Functions simplified.

# Version 1.0-1, September 3, 2010

* `pbreplicate` added.
* `/tests` directory created.
* `R CMD check` failed on unix systems:
  `/man` and `/R` directories reworked.

# Version 1.0-0, September 2, 2010

* First release.
