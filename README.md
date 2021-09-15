# pbapply: adding progress bar to '*apply' functions in R

[![CRAN version](http://www.r-pkg.org/badges/version/pbapply)](http://cran.rstudio.com/web/packages/pbapply/index.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/pbapply)](https://www.rdocumentation.org/packages/pbapply/)
[![Linux build status](https://travis-ci.org/psolymos/pbapply.svg?branch=master)](https://travis-ci.org/psolymos/pbapply)
[![Windows build status](https://ci.appveyor.com/api/projects/status/wnr13fj6ybis9jfy?svg=true)](https://ci.appveyor.com/project/psolymos/pbapply)
[![Code coverage status](https://codecov.io/gh/psolymos/pbapply/branch/master/graph/badge.svg)](https://codecov.io/gh/psolymos/pbapply)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![Research software impact](http://depsy.org/api/package/cran/pbapply/badge.svg)](http://depsy.org/package/r/pbapply)
[![Github Stars](https://img.shields.io/github/stars/psolymos/pbapply.svg?style=social&label=GitHub)](https://github.com/psolymos/pbapply)


A lightweight package that adds progress bar to vectorized R functions
(`*apply`). The implementation can easily be added to functions where showing the progress is
useful (e.g. bootstrap). The type and style of the progress bar (with percentages or remaining time) can be set through options.
The package supports several parallel processing backends, such as snow-type clusters and multicore-type forking
(see overview [here](http://peter.solymos.org/code/2016/09/11/what-is-the-cost-of-a-progress-bar-in-r.html)).

![](https://github.com/psolymos/pbapply/raw/master/images/pbapply-02.gif)

## Versions

Install CRAN release version (recommended):

```R
install.packages("pbapply")
```

Development version:

```R
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("psolymos/pbapply")
```

See user-visible changes in the [NEWS](https://github.com/psolymos/pbapply/blob/master/NEWS.md) file.

Use the [issue tracker](https://github.com/psolymos/pbapply/issues)
to report a problem, or to suggest a new feature.

## How to get started?

#### 1. You are not yet an R user

In this case, start with understaning basic programming concepts,
such as data structures (matrices, data frames, indexing these),
`for` loops and functions in R.
The online version of Garrett Grolemund's
[_Hands-On Programming with R_](https://rstudio-education.github.io/hopr/) 
walks you through these concepts nicely.

#### 2. You are an R user but haven't used vectorized functions yet

Learn about vectorized functions designed to replace `for` loops:
`lapply`, `sapply`, and `apply`.
Here is a repository called 
[_The Road to Progress_](https://github.com/psolymos/the-road-to-progress) 
that I created to show how to got from a `for` loop to `lapply`/`sapply`.

#### 3. You are an R user familiar with vectorized functions

In this case, you can simply add `pbapply::pb` before your `*apply`
functions, e.g. `apply()` will be `pbapply::pbapply()`, etc.
You can guess what happens.
Now if you want to speed things up a little (or a lot),
try `pbapply::pbapply(..., cl = 4)` to use 4 cores instead of 1.

If you are a Windows user, things get a bit more complicated, but not much.
Check how to work with `parallel::parLapply` to set up a snow type cluster.
Have a look at the 
[_The Road to Progress_](https://github.com/psolymos/the-road-to-progress) 
repository to see a worked example.

#### 4. You are a seasoned R developer writing your own packages

Read on, the next section is for you.

## How to add pbapply to a package

There are two ways of adding the pbapply package to another package.

#### 1. Suggests: pbapply

Add pbapply to the Suggests field in the `DESCRIPTION`.

Use a conditional statement in your code to fall back on a base function in case of pbapply not installed:

```R
out <- if (requireNamespace("pbapply", quietly = TRUE)) {
   pbapply::pblapply(X, FUN, ...)
} else {
   lapply(X, FUN, ...)
}
```

See a small example package [here](https://github.com/psolymos/pbapplySuggests).

#### 2. Depends/Imports: pbapply

Add pbapply to the Depends or Imports field in the `DESCRIPTION`.

Use the pbapply functions either as `pbapply::pblapply()` or specify them in the `NAMESPACE` (`importFrom(pbapply, pblapply)`) and
use it as `pblapply()` (without the `::`).

#### Customizing the progress bar in your package

Specify the progress bar options in the `zzz.R` file of the package:

```R
.onAttach <- function(libname, pkgname){
    options("pboptions" = list(
        type = if (interactive()) "timer" else "none",
        char = "-",
        txt.width = 50,
        gui.width = 300,
        style = 3,
        initial = 0,
        title = "R progress bar",
        label = "",
        nout = 100L,
        min_time = 2))
    invisible(NULL)
}
```

This will set the options and pbapply will not override when loaded.

See a small example package [here](https://github.com/psolymos/pbapplyDepends).

#### Suppressing the progress bar in your functions

Suppressing the progress bar is sometimes handy. By default, progress bar is suppressed when `!interactive()`.
In other instances, put this inside a function:

```R
pbo <- pboptions(type = "none")
on.exit(pboptions(pbo), add = TRUE)
```

## Examples

### Command line

```R
library(pbapply)
set.seed(1234)
n <- 2000
x <- rnorm(n)
y <- rnorm(n, model.matrix(~x) %*% c(0,1), sd=0.5)
d <- data.frame(y, x)
## model fitting and bootstrap
mod <- lm(y~x, d)
ndat <- model.frame(mod)
B <- 500
bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
fun <- function(z) {
    if (missing(z))
        z <- sample(nrow(ndat), nrow(ndat), TRUE)
    coef(lm(mod$call$formula, data=ndat[z,]))
}

## standard '*apply' functions
# system.time(res1 <- lapply(1:B, function(i) fun(bid[,i])))
#    user  system elapsed
#   1.096   0.023   1.127
system.time(res2 <- sapply(1:B, function(i) fun(bid[,i])))
#    user  system elapsed
#   1.152   0.017   1.182
system.time(res3 <- apply(bid, 2, fun))
#    user  system elapsed
#   1.134   0.010   1.160
system.time(res4 <- replicate(B, fun()))
#    user  system elapsed
#   1.141   0.022   1.171

## 'pb*apply' functions
## try different settings:
## "none", "txt", "tk", "win", "timer"
op <- pboptions(type="timer") # default
system.time(res1pb <- pblapply(1:B, function(i) fun(bid[,i])))
#    |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% ~00s
#    user  system elapsed
#   1.539   0.046   1.599
pboptions(op)

pboptions(type="txt")
system.time(res2pb <- pbsapply(1:B, function(i) fun(bid[,i])))
#   |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
#    user  system elapsed
#   1.433   0.045   1.518
pboptions(op)

pboptions(type="txt", style=1, char="=")
system.time(res3pb <- pbapply(bid, 2, fun))
# ==================================================
#    user  system elapsed
#   1.389   0.032   1.464
pboptions(op)

pboptions(type="txt", char=":")
system.time(res4pb <- pbreplicate(B, fun()))
#   |::::::::::::::::::::::::::::::::::::::::::::::::::| 100%
#    user  system elapsed
#   1.427   0.040   1.481
pboptions(op)
```

### Shiny

```R
library(shiny)
library(pbapply)

pboptions(
    type = "shiny",
    title = "Shiny progress",
    label = "Almost there ...")

ui <- fluidPage(
    plotOutput("plot")
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        pbsapply(1:15, function(z) Sys.sleep(0.5))
        plot(cars)
    })
}

shinyApp(ui, server)
```

![Shiny session](https://github.com/psolymos/pbapply/raw/master/images/shiny.png)
