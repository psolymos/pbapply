# Creating Progress Bar and Setting Options

Creating progress bar and setting options.

## Usage

``` r
pboptions(...)
startpb(min = 0, max = 1)
setpb(pb, value)
getpb(pb)
closepb(pb)
dopb()
doshiny()
pbtypes()
```

## Arguments

- ...:

  Arguments in `tag = value` form, or a list of tagged values. The tags
  must come from the parameters described below.

- pb:

  A progress bar object created by `startpb`.

- min, max:

  Finite numeric values for the extremes of the progress bar. Must have
  `min < max`.

- value:

  New value for the progress bar.

## Details

`pboptions` is a convenient way of handling options related to progress
bar.

Other functions can be used for conveniently adding progress bar to
`for`-like loops (see Examples).

## Value

When parameters are set by `pboptions`, their former values are returned
in an invisible named list. Such a list can be passed as an argument to
`pboptions` to restore the parameter values. Tags are the following:

- type:

  Type of the progress bar: timer (`"timer"`), text (`"txt"`), Windows
  (`"win"`), TclTk (`"tk"`), none (`"none"`), or Shiny (`"shiny"`).
  Default value is `"timer"` progress bar with estimated remaining time
  when in interactive mode, and `"none"` otherwise. See `pbtypes()` for
  available progress bar types depending on operating system.

- char:

  The character (or character string) to form the progress bar. Default
  value is `"+"`.

- txt.width:

  The width of the text based progress bar, as a multiple of the width
  of `char`. If `NA`, the number of characters is that which fits into
  `getOption("width")`. Default value is `50`.

- gui.width:

  The width of the GUI based progress bar in pixels: the dialogue box
  will be 40 pixels wider (plus frame). Default value is `300`.

- style:

  The style of the bar, see
  [`txtProgressBar`](https://rdrr.io/r/utils/txtProgressBar.html) and
  [`timerProgressBar`](timerProgressBar.md). Default value is `3`.

- initial:

  Initial value for the progress bar. Default value is `0`.

- title:

  Character string giving the window title on the GUI dialogue box.
  Default value is `"R progress bar"`.

- label:

  Character string giving the window label on the GUI dialogue box.
  Default value is `""`.

- nout:

  Integer, the maximum number of times the progress bar is updated. The
  default value is 100. Smaller value minimizes the running time
  overhead related to updating the progress bar. This can be especially
  important for forking type parallel runs.

- min_time:

  Minimum time in seconds. [`timerProgressBar`](timerProgressBar.md)
  output is printed only if estimated completion time is higher than
  this value. The default value is 0.

- use_lb:

  Switch for using load balancing when running in parallel clusters. The
  default value is `FALSE`.

For `startpb` a progress bar object.

For `getpb` and `setpb`, a length-one numeric vector giving the previous
value (invisibly for `setpb`). The return value is `NULL` if the
progress bar is turned off by `getOption("pboptions")$type` (`"none"` or
`NULL` value).

`dopb` returns a logical value if progress bar is to be shown based on
the option `getOption("pboptions")$type`. It is `FALSE` if the type of
progress bar is `"none"` or `NULL`.

`doshiny` returns a logical value, `TRUE` when the shiny package
namespace is available (i.e. the suggested package is installed), the
`type` option is set to `"shiny"`, and a shiny application is running.

For `closepb` closes the connection for the progress bar.

`pbtypes` prints the available progress bar types depending on the
operating system (i.e. `"win"` available on Windows only).

## Author

Peter Solymos \<solymos@ualberta.ca\>

## See also

Progress bars used in the functions:
[`timerProgressBar`](timerProgressBar.md),
[`txtProgressBar`](https://rdrr.io/r/utils/txtProgressBar.html),
[`tkProgressBar`](https://rdrr.io/r/tcltk/tkProgressBar.html)

## Examples

``` r
## increase sluggishness to admire the progress bar longer
sluggishness <- 0.01

## for loop
fun1 <- function() {
    pb <- startpb(0, 10)
    on.exit(closepb(pb))
    for (i in 1:10) {
        Sys.sleep(sluggishness)
        setpb(pb, i)
    }
    invisible(NULL)
}
## while loop
fun2 <- function() {
    pb <- startpb(0, 10-1)
    on.exit(closepb(pb))
    i <- 1
    while (i < 10) {
        Sys.sleep(sluggishness)
        setpb(pb, i)
        i <- i + 1
    }
    invisible(NULL)
}
## using original settings
fun1()
## resetting pboptions
opb <- pboptions(style = 1, char = ">")
## check new settings
getOption("pboptions")
#> $type
#> [1] "none"
#> 
#> $char
#> [1] ">"
#> 
#> $txt.width
#> [1] 50
#> 
#> $gui.width
#> [1] 300
#> 
#> $style
#> [1] 1
#> 
#> $initial
#> [1] 0
#> 
#> $title
#> [1] "R progress bar"
#> 
#> $label
#> [1] ""
#> 
#> $nout
#> [1] 100
#> 
#> $min_time
#> [1] 0
#> 
#> $use_lb
#> [1] FALSE
#> 
## running again with new settings
fun2()
## resetting original
pboptions(opb)
## check reset
getOption("pboptions")
#> $type
#> [1] "none"
#> 
#> $char
#> [1] "+"
#> 
#> $txt.width
#> [1] 50
#> 
#> $gui.width
#> [1] 300
#> 
#> $style
#> [1] 3
#> 
#> $initial
#> [1] 0
#> 
#> $title
#> [1] "R progress bar"
#> 
#> $label
#> [1] ""
#> 
#> $nout
#> [1] 100
#> 
#> $min_time
#> [1] 0
#> 
#> $use_lb
#> [1] FALSE
#> 
fun1()

## dealing with nested progress bars
## when only one the 1st one is needed
f <- function(x) Sys.sleep(sluggishness)
g <- function(x) pblapply(1:10, f)
tmp <- lapply(1:10, g) # undesirable
## here is the desirable solution
h <- function(x) {
    opb <- pboptions(type="none")
    on.exit(pboptions(opb))
    pblapply(1:10, f)
}
tmp <- pblapply(1:10, h)

## list available pb types
pbtypes()
#> [1] "timer" "txt"   "tk"    "none"  "shiny"
```
