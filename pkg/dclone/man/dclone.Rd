\encoding{UTF-8}
\name{dclone}
\Rdversion{1.1}
\alias{dclone}
\alias{dclone.default}
\alias{dclone.list}
\alias{dclone.ts}
\alias{dcsd}
\title{
Clone R Objects
}
\description{
Makes clones of R objects, that is values in the object are repeated \eqn{n} times, 
leaving the original structure of the object intact.
}
\usage{
dclone(x, n.clones=1, ...)
\method{dclone}{default}(x, n.clones = 1, attrib=TRUE, ...)
\method{dclone}{ts}(x, n.clones = 1, attrib=TRUE, ...)
\method{dclone}{list}(x, n.clones = 1, 
multiply = NULL, unchanged = NULL, attrib=TRUE, ...)
dcsd(x, na.rm = FALSE)
}
\arguments{
  \item{x}{
An R object to be cloned, or a cloned object to print.
}
  \item{n.clones}{
Number of clones.
}
  \item{multiply}{
Numeric or character index for list element(s) to be multiplied by \code{n.clones}
instead of repetitions (as done by \code{dclone.default}).
}
  \item{unchanged}{
Numeric or character index for list element(s) to be left unchanged.
}
  \item{attrib}{
Logical, \code{TRUE} if attributes are to be attached.
}
  \item{na.rm}{
Logical, if \code{NA}s should be removed.
}
  \item{\dots}{
Other arguments passed to function.
}
}
\value{
An object with class attributes \code{"dclone"} plus the original one(s).
Dimensions of the original object might change according to \code{n.clones}.
The function tries to take care of names, sometimes replacing those with the
combination of the original names and an integer for number of clones.

\code{dcsd} returns the data cloning standard errors of a posterior MCMC chain
calculated as standard deviation times the square root of the number of clones.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\examples{
## scalar
dclone(4, 2)
## vector
(x <- 1:6)
dclone(x, 2)
## matrix
(m <- matrix(x, 2, 3))
dclone(m, 2)
## data frame
(dfr <- as.data.frame(t(m)))
dclone(dfr, 2)
## list
(l <- list(n = 10, y = 1:10, x = 1:10, p = 1))
dclone(l, 2)
dclone(l, 2, attrib = FALSE)
dclone(l, 2, multiply = "n", unchanged = "p")
## time series
z <- as.ts(rnorm(10))
dclone(z, 2)
}
\keyword{ manip }
