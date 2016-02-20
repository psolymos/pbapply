# pbapply: adding progress bar to '*apply' functions in R

A lightweight package that adds
progress bar to vectorized R functions
(`*apply`). The implementation can easily be added
to functions, where showing the progress is
useful for the user (e.g. bootstrap).

## Versions

### CRAN release version

[![CRAN version](http://www.r-pkg.org/badges/version/pbapply)](http://cran.rstudio.com/web/packages/pbapply/index.html) [![](http://cranlogs.r-pkg.org/badges/grand-total/pbapply)](http://cran.rstudio.com/web/packages/pbapply/index.html)

### Build status for development version

[![Build Status](https://travis-ci.org/psolymos/pbapply.svg?branch=master)](https://travis-ci.org/psolymos/pbapply)

## Report a problem

Use the [issue tracker](https://github.com/psolymos/pbapply/issues)
to report a problem.

## Example

```
> set.seed(1234)
> n <- 2000
> x <- rnorm(n)
> y <- rnorm(n, model.matrix(~x) %*% c(0,1), sd=0.5)
> d <- data.frame(y, x)
> ## model fitting and bootstrap
> mod <- lm(y~x, d)
> ndat <- model.frame(mod)
> B <- 500
> bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
> fun <- function(z) {
+     if (missing(z))
+         z <- sample(nrow(ndat), nrow(ndat), TRUE)
+     coef(lm(mod$call$formula, data=ndat[z,]))
+ }
> ## standard '*apply' functions
> system.time(res1 <- lapply(1:B, function(i) fun(bid[,i])))
   user  system elapsed
  1.110   0.014   1.134
> system.time(res2 <- sapply(1:B, function(i) fun(bid[,i])))
   user  system elapsed
  1.100   0.008   1.117
> system.time(res3 <- apply(bid, 2, fun))
   user  system elapsed
  1.224   0.010   1.276
>
> ## 'pb*apply' functions
> ## try different settings:
> ## "none", "txt", "tk", "win", "timer"
> op <- pboptions(type="timer") # default
> system.time(res4 <- pblapply(1:B, function(i) fun(bid[,i])))
   |++++++++++++++++++++++++++++++++++++++++++++++++++| 100.00% ~00s
   user  system elapsed
  1.457   0.045   1.505
> pboptions(op)
>
> pboptions(type="txt")
> system.time(res5 <- pbsapply(1:B, function(i) fun(bid[,i])))
  |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
   user  system elapsed
  1.446   0.044   1.527
> pboptions(op)
>
> pboptions(type="txt", style=1, char="=")
> system.time(res6 <- pbapply(bid, 2, fun))
==================================================
   user  system elapsed
  1.315   0.033   1.372
> pboptions(op)
>
> pboptions(type="txt", char=":")
> system.time(res7 <- pbreplicate(B, fun()))
  |::::::::::::::::::::::::::::::::::::::::::::::::::| 100%
   user  system elapsed
  1.446   0.043   1.566
> pboptions(op)
```
