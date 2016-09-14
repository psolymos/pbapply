\name{pbapply}
\alias{pbapply}
\alias{pbsapply}
\alias{pblapply}
\alias{pbreplicate}
\title{
Adding Progress Bar to '*apply' Functions
}
\description{
Adding progress bar to \code{*apply} functions, possibly leveraging
parallel processing.
}
\usage{
pblapply(X, FUN, ..., cl = NULL)
pbapply(X, MARGIN, FUN, ...)
pbsapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, cl = NULL)
pbreplicate(n, expr, simplify = "array", cl = NULL)
}
\arguments{
  \item{X}{
For \code{pbsapply} and \code{pblapply},
a vector (atomic or list) or an expressions vector
(other objects including classed objects will be
coerced by \code{\link{as.list}}.)
For \code{pbapply} an array, including a matrix.
}
  \item{MARGIN}{
A vector giving the subscripts which the function will be applied over.
\code{1} indicates rows, \code{2} indicates columns, \code{c(1,2)}
indicates rows and columns.
}
  \item{FUN}{
The function to be applied to each element of \code{X}:
see \code{\link{apply}}, \code{\link{sapply}}, and \code{\link{lapply}}.
}
  \item{\dots}{
Optional arguments to \code{FUN}.
}
  \item{simplify}{
Logical; should the result be simplified to a vector or matrix if possible?
}
  \item{USE.NAMES}{
Logical; if \code{TRUE} and if \code{X} is character,
use \code{X} as names for the result unless it had names already.
}
  \item{n}{
Number of replications.
}
  \item{expr}{
Expression (language object, usually a call) to evaluate repeatedly.
}
  \item{cl}{
A cluster object created by \code{\link[parallel]{makeCluster}},
or an integer to indicate number of child-processes
(integer values are ignored on Windows) for parallel evaluations.
}
}
\details{
The behaviour of the progress bar is controlled by the option
\code{type} in \code{\link{pboptions}},
it can take values \code{c("txt", "win", "tk", "none",)} on Windows,
and \code{c("txt", "tk", "none",)} on Unix systems.

Other options have elements that are arguments used in the functions
\code{\link{timerProgressBar}}, \code{\link[utils]{txtProgressBar}},
#ifdef windows
\code{\link[utils]{winProgressBar}},
#endif
and \code{\link[tcltk]{tkProgressBar}}.
See \code{\link{pboptions}} for how to conveniently set these.

Parallel processing can be enabled through the \code{cl} argument.
\code{\link[parallel]{parLapply}} is called when \code{cl} is a 'cluster' object,
\code{\link[parallel]{mclapply}} is called when \code{cl} is an integer.
Showing the progress bar increases the communication overhead
between the main process and nodes / child processes compared to the
parallel equivalents of the functions without the progress bar.
The functions fall back to their original equivalents when the progress bar is
disabled (i.e. \code{getOption("pboptions")$type == "none"} or \code{dopb()} is
\code{FALSE}). This is the default when \code{interactive()} if \code{FALSE}
(i.e. called from command line R script).

When doing parallel processing, other objects might need to pushed to the
workers, and random numbers must be handled with care (see Examples).
}
\value{
Similar to the value returned by the standard \code{*apply} functions.

A progress bar is showed as a side effect.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\note{
Progress bar can add an overhead to the computation.
}
\seealso{
Progress bars used in the functions:
#ifdef windows
\code{\link[utils]{winProgressBar}},
#endif
\code{\link[utils]{txtProgressBar}},
\code{\link[tcltk]{tkProgressBar}}

Sequential \code{*apply} functions:
\code{\link{apply}}, \code{\link{sapply}},
\code{\link{lapply}}, \code{\link{replicate}}

Parallel \code{*apply} functions from package 'parallel':
\code{\link[parallel]{parLapply}},
\code{\link[parallel]{mclapply}}.

Setting the options: \code{\link{pboptions}}

Conveniently add progress bar to \code{for}-like loops:
\code{\link{startpb}}, \code{\link{setpb}}, \code{\link{getpb}},
\code{\link{closepb}}
}
\examples{
## --- simple linear model simulation ---
set.seed(1234)
n <- 200
x <- rnorm(n)
y <- rnorm(n, crossprod(t(model.matrix(~ x)), c(0, 1)), sd = 0.5)
d <- data.frame(y, x)
## model fitting and bootstrap
mod <- lm(y ~ x, d)
ndat <- model.frame(mod)
B <- 100
bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
fun <- function(z) {
    if (missing(z))
        z <- sample(nrow(ndat), nrow(ndat), TRUE)
    coef(lm(mod$call$formula, data=ndat[z,]))
}

## standard '*apply' functions
system.time(res1 <- lapply(1:B, function(i) fun(bid[,i])))
system.time(res2 <- sapply(1:B, function(i) fun(bid[,i])))
system.time(res3 <- apply(bid, 2, fun))
system.time(res4 <- replicate(B, fun()))

## 'pb*apply' functions
## try different settings:
## "none", "txt", "tk", "win", "timer"
op <- pboptions(type = "timer") # default
system.time(res1pb <- pblapply(1:B, function(i) fun(bid[,i])))
pboptions(op)

pboptions(type = "txt")
system.time(res2pb <- pbsapply(1:B, function(i) fun(bid[,i])))
pboptions(op)

pboptions(type = "txt", style = 1, char = "=")
system.time(res3pb <- pbapply(bid, 2, fun))
pboptions(op)

pboptions(type = "txt", char = ":")
system.time(res4pb <- pbreplicate(B, fun()))
pboptions(op)

\dontrun{
## parallel evaluation using the parallel package
## (n = 2000 and B = 1000 will give visible timing differences)

library(parallel)
cl <- makeCluster(2L)
clusterExport(cl, c("fun", "mod", "ndat", "bid"))

## parallel with no progress bar: snow type cluster
## (RNG is set in the main process to define the object bid)
system.time(res1cl <- parLapply(cl = cl, 1:B, function(i) fun(bid[,i])))
system.time(res2cl <- parSapply(cl = cl, 1:B, function(i) fun(bid[,i])))

## parallel with  progress bar: snow type cluster
## (RNG is set in the main process to define the object bid)
system.time(res1pbcl <- pblapply(1:B, function(i) fun(bid[,i]), cl = cl))
system.time(res2pbcl <- pbsapply(1:B, function(i) fun(bid[,i]), cl = cl))
## (RNG needs to be set when not using bid)
parallel::clusterSetRNGStream(cl, iseed = 0L)
system.time(res4pbcl <- pbreplicate(B, fun(), cl = cl))

stopCluster(cl)

if (.Platform$OS.type != "windows") {
    ## parallel with no progress bar: multicore type forking
    ## (mc.set.seed = TRUE in parallel::mclapply by default)
    system.time(res2mc <- mclapply(1:B, function(i) fun(bid[,i]), mc.cores = 2L))
    ## parallel with  progress bar: multicore type forking
    ## (mc.set.seed = TRUE in parallel::mclapply by default)
    system.time(res1pbmc <- pblapply(1:B, function(i) fun(bid[,i]), cl = 2L))
    system.time(res2pbmc <- pbsapply(1:B, function(i) fun(bid[,i]), cl = 2L))
    system.time(res4pbmc <- pbreplicate(B, fun(), cl = 2L))
}
}

## --- Examples taken from standard '*apply' functions ---

## --- sapply, lapply, and replicate ---

require(stats); require(graphics)

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
pblapply(x, mean)
# median and quartiles for each list element
pblapply(x, quantile, probs = 1:3/4)
pbsapply(x, quantile)
i39 <- sapply(3:9, seq) # list of vectors
pbsapply(i39, fivenum)

## sapply(*, "array") -- artificial example
(v <- structure(10*(5:8), names = LETTERS[1:4]))
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- pbsapply(v, f2, y = 2*(1:5), simplify = "array"))

hist(pbreplicate(100, mean(rexp(10))))

## use of replicate() with parameters:
foo <- function(x = 1, y = 2) c(x, y)
# does not work: bar <- function(n, ...) replicate(n, foo(...))
bar <- function(n, x) pbreplicate(n, foo(x = x))
bar(5, x = 3)

## --- apply ---

## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
pbapply(x, 2, mean, trim = .2)
col.sums <- pbapply(x, 2, sum)
row.sums <- pbapply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

stopifnot( pbapply(x, 2, is.vector))

## Sort the columns of a matrix
pbapply(x, 2, sort)

## keeping named dimnames
names(dimnames(x)) <- c("row", "col")
x3 <- array(x, dim = c(dim(x),3),
	    dimnames = c(dimnames(x), list(C = paste0("cop.",1:3))))
identical(x,  pbapply( x,  2,  identity))
identical(x3, pbapply(x3, 2:3, identity))

##- function with extra args:
cave <- function(x, c1, c2) c(mean(x[c1]), mean(x[c2]))
pbapply(x, 1, cave,  c1 = "x1", c2 = c("x1","x2"))

ma <- matrix(c(1:4, 1, 6:8), nrow = 2)
ma
pbapply(ma, 1, table)  #--> a list of length 2
pbapply(ma, 1, stats::quantile) # 5 x n matrix with rownames

stopifnot(dim(ma) == dim(pbapply(ma, 1:2, sum)))

## Example with different lengths for each call
z <- array(1:24, dim = 2:4)
zseq <- pbapply(z, 1:2, function(x) seq_len(max(x)))
zseq         ## a 2 x 3 matrix
typeof(zseq) ## list
dim(zseq) ## 2 3
zseq[1,]
pbapply(z, 3, function(x) seq_len(max(x)))
# a list without a dim attribute
}
\keyword{ manip }
\keyword{ utilities }
