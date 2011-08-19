\encoding{UTF-8}
\name{dclone}
\Rdversion{1.1}
\alias{dclone}
\alias{dclone.default}
\alias{dclone.list}
\alias{dclone.dcdim}
\alias{dcdim}
\title{
Cloning R objects
}
\description{
Makes clones of R objects, that is values in the object are repeated \eqn{n} times, 
leaving the original structure of the object intact (in most of the cases).
}
\usage{
dclone(x, n.clones=1, ...)
\method{dclone}{default}(x, n.clones = 1, attrib=TRUE, ...)
\method{dclone}{dcdim}(x, n.clones = 1, attrib=TRUE, ...)
\method{dclone}{list}(x, n.clones = 1, 
multiply = NULL, unchanged = NULL, attrib=TRUE, ...)
dcdim(x, drop = TRUE, perm = NULL)
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
  \item{drop}{
Logical, if \code{TRUE}, deletes the last dimension of an array if that have only one level.
}
  \item{perm}{
The subscript permutation value, if the cloning dimension is not the last.
}
  \item{\dots}{
Other arguments passed to function.
}
}
\details{
\code{dclone} is a generic method to clone objects. It is separate from \code{\link{rep}},
because there are different ways of cloning, depending on the \code{BUGS} code implementation:

(1) Unchanged: no cloning at all (fo e.g. constants).

(2) Repeat: this is the most often used cloning method, repeating the observations as if there
were more samples.

(3) Multiply: sometimes it is enough to multiply the numbers (e.g. for Binomial distribution).

(4) Add dimension: under specific circumstances, it is easier to add another dimension for clones,
but othervise repeat the observations (e.g. in case of time series, or for addressing special
indexing conventions in the \code{BUGS} code, see examples \code{dcdim} and \code{dclone.dcdim}).
}
\value{
An object with class attributes \code{"dclone"} plus the original one(s).
Dimensions of the original object might change according to \code{n.clones}.
The function tries to take care of names, sometimes replacing those with the
combination of the original names and an integer for number of clones.

\code{dcdim} sets the class attribute of an object to \code{"dcdim"}, thus
\code{dclone} will clone the object by adding an extra dimension for the clones.

List (i.e. BUGS data objects) are handled differently to enable element specific
determination of the mode of cloning. This can be done via the \code{unchanged}
and \code{multiply} arguments, or by setting the behaviour by the \code{dcdim} function.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized linear mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{105}, 1617--1625.

\enc{So'lymos}{Solymos}, P., 2010. dclone: Data Cloning in R.
\emph{The R Journal} \strong{2(2)}, 29--37.
URL: \url{http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}
}
\author{
\enc{Pe'ter So'lymos}{Peter Solymos}, \email{solymos@ualberta.ca}, 
implementation is based on many discussions with Khurram Nadeem and Subhash Lele.
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
## effect of dcdim
l$y <- dcdim(l$y)
dclone(l, 2, multiply = "n", unchanged = "p")
## time series like usage of dcdim
z <- data.matrix(rnorm(10))
dclone(dcdim(z), 2)
}
\keyword{ manip }
