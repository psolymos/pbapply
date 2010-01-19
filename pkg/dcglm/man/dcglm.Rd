\name{dcglm}
\alias{dcglm}
\title{
Data Cloning Version of the glm Base R Function
}
\description{
Data cloning version of the \code{\link{glm}} base R function.
It uses the data cloning algorithm (Lele et al. 2007) as numerical optimization.
}
\usage{
dcglm(formula, data = parent.frame(), family = c("poisson", "binomial"), n.clones = 5, ...)
}
\arguments{
  \item{formula}{
an object of class '"formula"' (or one that can be coerced to
that class): a symbolic description of the model to be
fitted.  The details of model specification are given under
\code{\link{glm}}.
}
  \item{data}{
an optional data frame, list or environment (or object
coercible by 'as.data.frame' to a data frame) containing the
variables in the model.
}
  \item{family}{
a description of the error distribution.
}
  \item{n.clones}{
number of clones to use in the data cloning.
}
  \item{\dots}{
additional arguments passed to \code{\link{jags.fit}}, such as \code{n.update},
\code{n.iter}, \code{n.chains} and \code{thin}.
}
}
\value{
An object of class 'dcglm', similar to the object returned by \code{\link{glm}}.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
Peter Solymos \email{solymos@ulaberta.ca}
}
\seealso{
\code{\link{glm}}.
}
\examples{
## see vignette
}
\keyword{ models }
