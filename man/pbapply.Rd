\name{pbapply}
\alias{pbapply}
\alias{pbsapply}
\alias{pblapply}
\alias{pbwalk}
\alias{pbreplicate}
\alias{pbmapply}
\alias{pb.mapply}
\alias{pbtapply}
\alias{pbeapply}
\alias{pbvapply}
\alias{pbby}
\alias{pbMap}
\title{
Adding Progress Bar to '*apply' Functions
}
\description{
Adding progress bar to \code{*apply} functions, possibly leveraging
parallel processing.
}
\usage{
pblapply(X, FUN, ..., cl = NULL)
pbeapply(env, FUN, ..., all.names = FALSE, USE.NAMES = TRUE, cl = NULL)
pbwalk(X, FUN, ..., cl = NULL)

pbapply(X, MARGIN, FUN, ..., simplify = TRUE, cl = NULL)

pbsapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, cl = NULL)
pbvapply(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE, cl = NULL)
pbreplicate(n, expr, simplify = "array", cl = NULL)

pbmapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
pb.mapply(FUN, dots, MoreArgs)
pbMap(f, ...)

pbtapply(X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE, cl = NULL)

pbby(data, INDICES, FUN, ..., simplify = TRUE, cl = NULL)
}
\arguments{
  \item{X}{
For \code{pbsapply}, \code{pblapply}, and \code{pbwalk}
a vector (atomic or list) or an expressions vector
(other objects including classed objects will be
coerced by \code{\link{as.list}}.)
For \code{pbapply} an array, including a matrix.
For \code{pbtapply} an R object for which a \code{\link{split}} method exists.
Typically vector-like, allowing subsetting with \code{\link{[}}.
}
  \item{MARGIN}{
A vector giving the subscripts which the function will be applied over.
\code{1} indicates rows, \code{2} indicates columns, \code{c(1,2)}
indicates rows and columns.
}
  \item{FUN, f}{
The function to be applied to each element of \code{X}:
see \code{\link{apply}}, \code{\link{sapply}}, and \code{\link{lapply}}.
In the case of functions like \code{+}, \code{\%*\%}, etc.,
the function name must be backquoted or quoted. If \code{FUN} is \code{NULL},
\code{pbtapply} returns a vector which can be used to subscript the
multi-way array \code{pbtapply} normally produces.
}
  \item{\dots}{
Optional arguments to \code{FUN} and also to underlying functions (e.g.
\code{\link{parLapply}} and \code{\link{mclapply}} when \code{cl}
is not \code{NULL}).
}
  \item{dots}{
List of arguments to vectorize over (vectors or lists
of strictly positive length, or all of zero length);
see \code{\link{.mapply}}.
}
  \item{env}{
Environment to be used.
}
  \item{FUN.VALUE}{
A (generalized) vector; a template for the return value from \code{FUN}.
See 'Details' for \code{\link{vapply}}.
}
  \item{simplify, SIMPLIFY}{
Logical; should the result be simplified to a vector or matrix if possible?
\code{pbtapply} returns an array of mode \code{"list"} (in other words,
a list with a dim attribute) when \code{FALSE}; if \code{TRUE} (the default),
then if \code{FUN} always returns a scalar, \code{pbtapply} returns an array with
the mode of the scalar.
}
  \item{USE.NAMES}{
Logical; if \code{TRUE} and if \code{X} is character,
use \code{X} as names for the result unless it had names already.
}
  \item{all.names}{
Logical, indicating whether to apply the function to all values.
}
  \item{n}{
Number of replications.
}
  \item{expr}{
Expression (language object, usually a call) to evaluate repeatedly.
}
  \item{cl}{
A cluster object created by \code{\link{makeCluster}},
or an integer to indicate number of child-processes
(integer values are ignored on Windows) for parallel evaluations
(see Details on performance).
It can also be \code{"future"} to use a future backend,
\code{NULL} (default) refers to sequential evaluation.
}
  \item{MoreArgs}{
A list of other arguments to \code{FUN}.
}
  \item{INDEX}{
A \code{\link{list}} of one or more \code{\link{factor}}s, each of same length
as \code{X}. The elements are coerced to factors by \code{\link{as.factor}}.
}
  \item{INDICES}{
A factor or a list of factors, each of length \code{nrow(data)}.
}
  \item{data}{
An R object, normally a data frame, possibly a matrix.
}
  \item{default}{
Only in the case of simplification to an array, the value with which the array
is initialized as \code{\link{array}}\code{(default, dim = ..)}.
Before R 3.4.0, this was hard coded to \code{\link{array}}\code{()}'s
default \code{NA}. If it is \code{NA} (the default), the missing value of the
answer type, e.g. \code{\link{NA_real_}}, is chosen
(\code{\link{as.raw}}\code{(0)} for \code{"raw"}). In a numerical case,
it may be set, e.g., to \code{FUN(integer(0))}, e.g., in the case of
\code{FUN = sum} to \code{0} or \code{0L}.
}
}
\details{
The behavior of the progress bar is controlled by the option
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
\code{\link{parLapply}} is called when \code{cl} is a 'cluster' object,
\code{\link{mclapply}} is called when \code{cl} is an integer.
Showing the progress bar increases the communication overhead
between the main process and nodes / child processes compared to the
parallel equivalents of the functions without the progress bar.
The functions fall back to their original equivalents when the progress bar is
disabled (i.e. \code{getOption("pboptions")$type == "none"} or \code{dopb()} is
\code{FALSE}). This is the default when \code{interactive()} if \code{FALSE}
(i.e. called from command line R script).

When doing parallel processing, other objects might need to pushed to the
workers, and random numbers must be handled with care (see Examples).

Updating the progress bar with \code{\link{mclapply}}
can be slightly slower compared to using a Fork cluster
(i.e. calling \code{\link{makeForkCluster}}).
Care must be taken to set appropriate random numbers in this case.

Note the \code{use_lb} option (see \code{\link{pboptions}})
for using load balancing when running in parallel clusters.
If using \code{\link{mclapply}}, the \code{...} passes
arguments to the underlying function for further control.

\code{pbwalk} is similar to \code{pblapply} but it calls \code{FUN}
only for its side-effect and returns the input \code{X} invisibly
(this behavior is modeled after `purrr::walk`).
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
\code{\link[tcltk]{tkProgressBar}},
\code{\link{timerProgressBar}}

Sequential \code{*apply} functions:
\code{\link{apply}}, \code{\link{sapply}},
\code{\link{lapply}}, \code{\link{replicate}},
\code{\link{mapply}}, \code{\link{.mapply}},
\code{\link{tapply}}

Parallel \code{*apply} functions from package 'parallel':
\code{\link{parLapply}},
\code{\link{mclapply}}.

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
system.time(res3cl <- parApply(cl, bid, 2, fun))

## parallel with  progress bar: snow type cluster
## (RNG is set in the main process to define the object bid)
system.time(res1pbcl <- pblapply(1:B, function(i) fun(bid[,i]), cl = cl))
system.time(res2pbcl <- pbsapply(1:B, function(i) fun(bid[,i]), cl = cl))
## (RNG needs to be set when not using bid)
parallel::clusterSetRNGStream(cl, iseed = 0L)
system.time(res4pbcl <- pbreplicate(B, fun(), cl = cl))
system.time(res3pbcl <- pbapply(bid, 2, fun, cl = cl))

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
pbwalk(x, mean)
# median and quartiles for each list element
pblapply(x, quantile, probs = 1:3/4)
pbsapply(x, quantile)
i39 <- sapply(3:9, seq) # list of vectors
pbsapply(i39, fivenum)
pbvapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## sapply(*, "array") -- artificial example
(v <- structure(10*(5:8), names = LETTERS[1:4]))
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- pbsapply(v, f2, y = 2*(1:5), simplify = "array"))
a.2 <- pbvapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
          identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))

summary(pbreplicate(100, mean(rexp(10))))

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

## --- mapply and .mapply ---

pbmapply(rep, 1:4, 4:1)
pbmapply(rep, times = 1:4, x = 4:1)
pbmapply(rep, times = 1:4, MoreArgs = list(x = 42))
pbmapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))
word <- function(C, k) paste(rep.int(C, k), collapse = "")
utils::str(pbmapply(word, LETTERS[1:6], 6:1, SIMPLIFY = FALSE))

pb.mapply(rep,
          dots = list(1:4, 4:1),
          MoreArgs = list())
pb.mapply(rep,
          dots = list(times = 1:4, x = 4:1),
          MoreArgs = list())
pb.mapply(rep,
          dots = list(times = 1:4),
          MoreArgs = list(x = 42))
pb.mapply(function(x, y) seq_len(x) + y,
          dots = list(c(a =  1, b = 2, c = 3),  # names from first
                      c(A = 10, B = 0, C = -10)),
          MoreArgs = list())

## --- Map ---

pbMap(`+`, 1,         1 : 3) ;         1 + 1:3
pbMap(`+`, numeric(), 1 : 3) ; numeric() + 1:3

## --- eapply ---

env <- new.env(hash = FALSE)
env$a <- 1:10
env$beta <- exp(-3:3)
env$logic <- c(TRUE, FALSE, FALSE, TRUE)
pbeapply(env, mean)
unlist(pbeapply(env, mean, USE.NAMES = FALSE))
pbeapply(env, quantile, probs = 1:3/4)
pbeapply(env, quantile)

## --- tapply ---

require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
pbtapply(groups, groups, length) #- is almost the same as
table(groups)

## contingency table from data.frame : array with named dimnames
pbtapply(warpbreaks$breaks, warpbreaks[,-1], sum)
pbtapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)

n <- 17; fac <- factor(rep_len(1:3, n), levels = 1:5)
table(fac)
pbtapply(1:n, fac, sum)
pbtapply(1:n, fac, sum, default = 0) # maybe more desirable
pbtapply(1:n, fac, sum, simplify = FALSE)
pbtapply(1:n, fac, range)
pbtapply(1:n, fac, quantile)
pbtapply(1:n, fac, length) ## NA's
pbtapply(1:n, fac, length, default = 0) # == table(fac)

## example of ... argument: find quarterly means
pbtapply(presidents, cycle(presidents), mean, na.rm = TRUE)

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
pbtapply(1:3, ind) #-> the split vector
pbtapply(1:3, ind, sum)

## Some assertions (not held by all patch propsals):
nq <- names(quantile(1:5))
stopifnot(
  identical(pbtapply(1:3, ind), c(1L, 2L, 4L)),
  identical(pbtapply(1:3, ind, sum),
            matrix(c(1L, 2L, NA, 3L), 2, dimnames = list(c("1", "2"), c("A", "B")))),
  identical(pbtapply(1:n, fac, quantile)[-1],
            array(list(`2` = structure(c(2, 5.75, 9.5, 13.25, 17), .Names = nq),
                 `3` = structure(c(3, 6, 9, 12, 15), .Names = nq),
                 `4` = NULL, `5` = NULL), dim=4, dimnames=list(as.character(2:5)))))

## --- by ---

pbby(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
pbby(warpbreaks[, 1],   warpbreaks[, -1],       summary)
pbby(warpbreaks, warpbreaks[,"tension"],
   function(x) lm(breaks ~ wool, data = x))
tmp <- with(warpbreaks,
            pbby(warpbreaks, tension,
               function(x) lm(breaks ~ wool, data = x)))
sapply(tmp, coef)
}
\keyword{ manip }
\keyword{ utilities }
