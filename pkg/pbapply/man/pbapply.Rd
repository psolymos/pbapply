\name{pbapply}
\alias{pbapply}
\alias{pbsapply}
\alias{pblapply}
\title{
Adding Progress Bar to '*apply' Functions
}
\description{
Adding progress bar to \code{*apply} functions}
\usage{
pblapply(X, FUN, ...)
pbapply(X, MARGIN, FUN, ...)
pbsapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
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
}
\details{
The behaviour of the progress bar is controlled by the options:
\code{getOption("pbapply.pb")}, 
\code{getOption("pbapply.txt")}, 
and \code{getOption("pbapply.win")}. 

The \code{"pbapply.pb"} option can take values in
\code{c("txt", "win", "tk", "none",)} or can be \code{NULL}.
Other options have elements that are arguments used in the functions
\code{\link[utils]{winProgressBar}} (for \code{"txt"}), 
\code{\link[utils]{txtProgressBar}}, 
and \code{\link[tcltk]{tkProgressBar}} (both for \code{"gui"}).

See \code{\link{pboptions}} for how to conveniently set these.
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
\code{\link[utils]{winProgressBar}}, 
\code{\link[utils]{txtProgressBar}}, 
\code{\link[tcltk]{tkProgressBar}}

Standard \code{*apply} functions:
\code{\link{apply}}, \code{\link{sapply}}, \code{\link{lapply}}

Setting the options: \code{\link{pboptions}}

Conveniently add progress bar to \code{for}-like loops:
\code{\link{startpb}}, \code{\link{setpb}}, \code{\link{getpb}}, \code{\link{closepb}}
}
\examples{
\dontrun{
## simple linear model simulation
n <- 200
x <- rnorm(n)
y <- rnorm(n, model.matrix(~x) \%*\% c(0,1), sd=0.5)
d <- data.frame(y, x)
## model fitting and bootstrap
mod <- lm(y~x, d)
ndat <- model.frame(mod)
B <- 100
bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
fun <- function(z) {
    ndat <- ndat[sample(nrow(ndat), nrow(ndat), TRUE),]
    coef(lm(mod$call$formula, data=ndat[z,]))
}
## standard '*apply' functions
system.time(res1 <- lapply(1:B, function(i) fun(bid[,i])))
system.time(res2 <- sapply(1:B, function(i) fun(bid[,i])))
system.time(res3 <- apply(bid, 2, fun))
## 'pb*apply' functions
## try different settings:
## "none", "txt", "win", "tk"
options("pbapply.pb"="txt")
system.time(res4 <- pblapply(1:B, function(i) fun(bid[,i])))
system.time(res5 <- pbsapply(1:B, function(i) fun(bid[,i])))
system.time(res6 <- pbapply(bid, 2, fun))
}
}
\keyword{ manip }
\keyword{ utilities }
