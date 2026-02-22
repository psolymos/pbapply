# Timer Progress Bar

Text progress bar with timer in the R console.

## Usage

``` r
timerProgressBar(min = 0, max = 1, initial = 0, char = "=",
    width = NA, title, label, style = 1, file = "", min_time = 0)
getTimerProgressBar(pb)
setTimerProgressBar(pb, value, title = NULL, label = NULL)
getTimeAsString(time)
```

## Arguments

- min, max:

  (finite) numeric values for the extremes of the progress bar. Must
  have `min` \< `max`.

- initial, value:

  initial or new value for the progress bar. See Details for what
  happens with invalid values.

- char:

  he character (or character string) to form the progress bar. If number
  of characters is \>1, it is silently stripped to length 1 unless
  `style` is 5 or 6 (see Details).

- width:

  the width of the progress bar, as a multiple of the width of char. If
  `NA`, the default, the number of characters is that which fits into
  `getOption("width")`.

- style:

  the style taking values between 1 and 6. 1: progress bar with elapsed
  and remaining time, remaining percentage is indicated by spaces
  between pipes (default for this function), 2: throbber with elapsed
  and remaining time, 3: progress bar with remaining time printing
  elapsed time at the end, remaining percentage is indicated by spaces
  between pipes (default for `style` option in
  [`pboptions`](pboptions.md)), 4: throbber with remaining time printing
  elapsed time at the end, 5: progress bar with elapsed and remaining
  time with more flexible styling (see Details and Examples), 6:
  progress bar with remaining time printing elapsed time at the end with
  more flexible styling (see Details and Examples).

- file:

  an open connection object or `""` which indicates the console.

- min_time:

  numeric, minimum processing time (in seconds) required to show a
  progress bar.

- pb:

  an object of class `"timerProgressBar"`.

- title, label:

  ignored, for compatibility with other progress bars.

- time:

  numeric of length 1, time in seconds.

## Details

`timerProgressBar` will display a progress bar on the R console (or a
connection) via a text representation.

`setTimerProgessBar` will update the value. Missing (`NA`) and
out-of-range values of value will be (silently) ignored. (Such values of
`initial` cause the progress bar not to be displayed until a valid value
is set.)

The progress bar should be closed when finished with: this outputs the
final newline character (see [`closepb`](pboptions.md)).

If `style` is 5 or 6, it is possible to define up to 4 characters for
the `char` argument (as a single string) for the left end, elapsed
portion, remaining portion, and right end of the progress bar (`|= |` by
default). Remaining portion cannot be the same as the elapsed portion
(space is used for remaining in such cases). If 1 character is defined,
it is taken for the elapsed portion. If 2-4 characters are defined,
those are interpreted in sequence (left and right end being the same
when 2-3 characters defined), see Examples.

`getTimeAsString` converts time in seconds into ~HHh MMm SSs format to
be printed by `timerProgressBar`.

## Value

For `timerProgressBar` an object of class `"timerProgressBar"`
inheriting from `"txtProgressBar"`.

For `getTimerProgressBar` and `setTimerProgressBar`, a length-one
numeric vector giving the previous value (invisibly for
`setTimerProgressBar`).

`getTimeAsString` returns time in ~HHh MMm SSs format as character.
Returns `"calculating"` when `time=NULL`.

## Author

Zygmunt Zawadzki \<zawadzkizygmunt@gmail.com\>

Peter Solymos \<solymos@ualberta.ca\>

## See also

The `timerProgressBar` implementation follows closely the code of
[`txtProgressBar`](https://rdrr.io/r/utils/txtProgressBar.html).

## Examples

``` r
## increase sluggishness to admire the progress bar longer
sluggishness <- 0.02

test_fun <- function(...)
{
    pb <- timerProgressBar(...)
    on.exit(close(pb))
    for (i in seq(0, 1, 0.05)) {
        Sys.sleep(sluggishness)
        setTimerProgressBar(pb, i)
    }
    invisible(NULL)
}

## check the different styles
test_fun(width = 35, char = "+", style = 1)
#> 
  |                                   | 0 % elapsed=00s   
  |                                   | 0 % elapsed=00s   
  |++                                 | 5 % elapsed=00s, remaining~01s
  |++++                               | 10% elapsed=00s, remaining~01s
  |++++++                             | 15% elapsed=00s, remaining~01s
  |+++++++                            | 20% elapsed=00s, remaining~01s
  |+++++++++                          | 25% elapsed=00s, remaining~00s
  |+++++++++++                        | 30% elapsed=00s, remaining~00s
  |+++++++++++++                      | 35% elapsed=00s, remaining~00s
  |++++++++++++++                     | 40% elapsed=00s, remaining~00s
  |++++++++++++++++                   | 45% elapsed=00s, remaining~00s
  |++++++++++++++++++                 | 50% elapsed=00s, remaining~00s
  |++++++++++++++++++++               | 55% elapsed=00s, remaining~00s
  |++++++++++++++++++++++             | 60% elapsed=00s, remaining~00s
  |+++++++++++++++++++++++            | 65% elapsed=00s, remaining~00s
  |+++++++++++++++++++++++++          | 70% elapsed=00s, remaining~00s
  |+++++++++++++++++++++++++++        | 75% elapsed=01s, remaining~00s
  |++++++++++++++++++++++++++++       | 80% elapsed=01s, remaining~00s
  |++++++++++++++++++++++++++++++     | 85% elapsed=01s, remaining~00s
  |++++++++++++++++++++++++++++++++   | 90% elapsed=01s, remaining~00s
  |++++++++++++++++++++++++++++++++++ | 95% elapsed=01s, remaining~00s
  |+++++++++++++++++++++++++++++++++++| 100% elapsed=01s, remaining~00s
test_fun(style = 2)
#> 
 / 0 % elapsed=00s   
 / 0 % elapsed=00s   
 - 5 % elapsed=00s, remaining~01s
 \ 10% elapsed=00s, remaining~01s
 | 15% elapsed=00s, remaining~01s
 / 20% elapsed=00s, remaining~01s
 - 25% elapsed=00s, remaining~01s
 \ 30% elapsed=00s, remaining~01s
 | 35% elapsed=00s, remaining~00s
 / 40% elapsed=00s, remaining~00s
 - 45% elapsed=00s, remaining~00s
 \ 50% elapsed=00s, remaining~00s
 | 55% elapsed=00s, remaining~00s
 / 60% elapsed=00s, remaining~00s
 - 65% elapsed=00s, remaining~00s
 \ 70% elapsed=00s, remaining~00s
 | 75% elapsed=01s, remaining~00s
 / 80% elapsed=01s, remaining~00s
 - 85% elapsed=01s, remaining~00s
 \ 90% elapsed=01s, remaining~00s
 | 95% elapsed=01s, remaining~00s
 / 100% elapsed=01s, remaining~00s
test_fun(width = 50, char = ".", style = 3)
#> 
  |                                                  | 0 % ~calculating  
  |                                                  | 0 % ~calculating  
  |...                                               | 5 % ~01s          
  |.....                                             | 10% ~01s          
  |........                                          | 15% ~01s          
  |..........                                        | 20% ~01s          
  |.............                                     | 25% ~01s          
  |................                                  | 30% ~01s          
  |..................                                | 35% ~00s          
  |....................                              | 40% ~00s          
  |.......................                           | 45% ~00s          
  |.........................                         | 50% ~00s          
  |............................                      | 55% ~00s          
  |...............................                   | 60% ~00s          
  |.................................                 | 65% ~00s          
  |...................................               | 70% ~00s          
  |......................................            | 75% ~00s          
  |........................................          | 80% ~00s          
  |...........................................       | 85% ~00s          
  |.............................................     | 90% ~00s          
  |................................................  | 95% ~00s          
  |..................................................| 100% elapsed=01s  
test_fun(style = 4)
#> 
 / 0 % ~calculating  
 / 0 % ~calculating  
 - 5 % ~01s          
 \ 10% ~01s          
 | 15% ~01s          
 / 20% ~01s          
 - 25% ~01s          
 \ 30% ~01s          
 | 35% ~01s          
 / 40% ~00s          
 - 45% ~00s          
 \ 50% ~00s          
 | 55% ~00s          
 / 60% ~00s          
 - 65% ~00s          
 \ 70% ~00s          
 | 75% ~00s          
 / 80% ~00s          
 - 85% ~00s          
 \ 90% ~00s          
 | 95% ~00s          
 / 100% elapsed=01s  
test_fun(width = 35, char = "[=-]", style = 5)
#> 
  [-----------------------------------] 0 % elapsed=00s   
  [-----------------------------------] 0 % elapsed=00s   
  [==---------------------------------] 5 % elapsed=00s, remaining~01s
  [====-------------------------------] 10% elapsed=00s, remaining~01s
  [======-----------------------------] 15% elapsed=00s, remaining~01s
  [=======----------------------------] 20% elapsed=00s, remaining~01s
  [=========--------------------------] 25% elapsed=00s, remaining~01s
  [===========------------------------] 30% elapsed=00s, remaining~01s
  [=============----------------------] 35% elapsed=00s, remaining~01s
  [==============---------------------] 40% elapsed=00s, remaining~01s
  [================-------------------] 45% elapsed=00s, remaining~00s
  [==================-----------------] 50% elapsed=00s, remaining~00s
  [====================---------------] 55% elapsed=00s, remaining~00s
  [======================-------------] 60% elapsed=00s, remaining~00s
  [=======================------------] 65% elapsed=01s, remaining~00s
  [=========================----------] 70% elapsed=01s, remaining~00s
  [===========================--------] 75% elapsed=01s, remaining~00s
  [============================-------] 80% elapsed=01s, remaining~00s
  [==============================-----] 85% elapsed=01s, remaining~00s
  [================================---] 90% elapsed=01s, remaining~00s
  [==================================-] 95% elapsed=01s, remaining~00s
  [===================================] 100% elapsed=01s, remaining~00s
test_fun(width = 50, char = "{*.}", style = 6)
#> 
  {..................................................} 0 % ~calculating  
  {..................................................} 0 % ~calculating  
  {***...............................................} 5 % ~01s          
  {*****.............................................} 10% ~01s          
  {********..........................................} 15% ~01s          
  {**********........................................} 20% ~01s          
  {*************.....................................} 25% ~01s          
  {****************..................................} 30% ~00s          
  {******************................................} 35% ~00s          
  {********************..............................} 40% ~00s          
  {***********************...........................} 45% ~00s          
  {*************************.........................} 50% ~00s          
  {****************************......................} 55% ~00s          
  {*******************************...................} 60% ~00s          
  {*********************************.................} 65% ~00s          
  {***********************************...............} 70% ~00s          
  {**************************************............} 75% ~00s          
  {****************************************..........} 80% ~00s          
  {*******************************************.......} 85% ~00s          
  {*********************************************.....} 90% ~00s          
  {************************************************..} 95% ~00s          
  {**************************************************} 100% elapsed=01s  

## no bar only percent and elapsed
test_fun(width = 0, char = "    ", style = 6)
#> 
  0 % ~calculating  
  0 % ~calculating  
  5 % ~02s          
  10% ~01s          
  15% ~01s          
  20% ~01s          
  25% ~01s          
  30% ~01s          
  35% ~01s          
  40% ~01s          
  45% ~01s          
  50% ~00s          
  55% ~00s          
  60% ~00s          
  65% ~00s          
  70% ~00s          
  75% ~00s          
  80% ~00s          
  85% ~00s          
  90% ~00s          
  95% ~00s          
  100% elapsed=01s  

## this should produce a progress bar based on min_time
(elapsed <- system.time(test_fun(width = 35, min_time = 0))["elapsed"])
#> 
  |                                   | 0 % elapsed=00s   
  |                                   | 0 % elapsed=00s   
  |==                                 | 5 % elapsed=00s, remaining~01s
  |====                               | 10% elapsed=00s, remaining~01s
  |======                             | 15% elapsed=00s, remaining~01s
  |=======                            | 20% elapsed=00s, remaining~01s
  |=========                          | 25% elapsed=00s, remaining~01s
  |===========                        | 30% elapsed=00s, remaining~01s
  |=============                      | 35% elapsed=00s, remaining~01s
  |==============                     | 40% elapsed=00s, remaining~01s
  |================                   | 45% elapsed=00s, remaining~01s
  |==================                 | 50% elapsed=00s, remaining~00s
  |====================               | 55% elapsed=01s, remaining~00s
  |======================             | 60% elapsed=01s, remaining~00s
  |=======================            | 65% elapsed=01s, remaining~00s
  |=========================          | 70% elapsed=01s, remaining~00s
  |===========================        | 75% elapsed=01s, remaining~00s
  |============================       | 80% elapsed=01s, remaining~00s
  |==============================     | 85% elapsed=01s, remaining~00s
  |================================   | 90% elapsed=01s, remaining~00s
  |================================== | 95% elapsed=01s, remaining~00s
  |===================================| 100% elapsed=01s, remaining~00s
#> elapsed 
#>    0.86 
## this should not produce a progress bar based on min_time
system.time(test_fun(min_time = 2 * elapsed))["elapsed"]
#> 
  |====                                                                            | 5 % elapsed=00s, remaining~02s
  |========                                                                        | 10% elapsed=00s, remaining~01s
  |=============                                                                   | 15% elapsed=00s, remaining~01s
  |================                                                                | 20% elapsed=00s, remaining~01s
  |====================                                                            | 25% elapsed=00s, remaining~01s
  |=========================                                                       | 30% elapsed=00s, remaining~01s
  |=============================                                                   | 35% elapsed=00s, remaining~01s
  |================================                                                | 40% elapsed=00s, remaining~01s
  |====================================                                            | 45% elapsed=00s, remaining~00s
  |========================================                                        | 50% elapsed=00s, remaining~00s
  |============================================                                    | 55% elapsed=00s, remaining~00s
  |=================================================                               | 60% elapsed=00s, remaining~00s
  |====================================================                            | 65% elapsed=00s, remaining~00s
  |=========================================================                       | 70% elapsed=01s, remaining~00s
  |============================================================                    | 75% elapsed=01s, remaining~00s
  |================================================================                | 80% elapsed=01s, remaining~00s
  |====================================================================            | 85% elapsed=01s, remaining~00s
  |========================================================================        | 90% elapsed=01s, remaining~00s
  |============================================================================    | 95% elapsed=01s, remaining~00s
  |================================================================================| 100% elapsed=01s, remaining~00s
#> elapsed 
#>   0.724 

## time formatting
getTimeAsString(NULL)
#> [1] "calculating"
getTimeAsString(15)
#> [1] "15s"
getTimeAsString(65)
#> [1] "01m 05s"
getTimeAsString(6005)
#> [1] "01h 40m 05s"

## example usage of getTimeAsString, use sluggishness <- 1
n <- 10
t0 <- proc.time()[3]
ETA <- NULL
for (i in seq_len(n)) {
    cat(i, "/", n, "- ETA:", getTimeAsString(ETA))
    flush.console()
    Sys.sleep(sluggishness)
    dt <- proc.time()[3] - t0
    cat(" - elapsed:", getTimeAsString(dt), "\n")
    ETA <- (n - i) * dt / i
}
#> 1 / 10 - ETA: calculating - elapsed: 00s 
#> 2 / 10 - ETA: 00s - elapsed: 00s 
#> 3 / 10 - ETA: 00s - elapsed: 00s 
#> 4 / 10 - ETA: 00s - elapsed: 00s 
#> 5 / 10 - ETA: 00s - elapsed: 00s 
#> 6 / 10 - ETA: 00s - elapsed: 00s 
#> 7 / 10 - ETA: 00s - elapsed: 00s 
#> 8 / 10 - ETA: 00s - elapsed: 00s 
#> 9 / 10 - ETA: 00s - elapsed: 00s 
#> 10 / 10 - ETA: 00s - elapsed: 00s 
```
