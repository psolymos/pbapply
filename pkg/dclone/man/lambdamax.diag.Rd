\name{lambdamax.diag}
\Rdversion{1.1}
\alias{lambdamax.diag}
\title{
Maximum Eigenvalue of the Posterior Variance-Covariance Matrix
}
\description{
Calculates the maximum eigenvalue of the posterior variance-covariance matrix
}
\usage{
lambdamax.diag(x)
}
\arguments{
  \item{x}{
  an object of class \code{mcmc} or \code{mcmc.list}.
}
}
\details{
This diagnostics can be used to test for the data cloning convergence.
Asymptotically the posterior distribution of the parameters approaches
a degenerate multivariate normal distribution. As the distribution
is getting more degenerate, the maximal eigenvalue (\eqn{\lambda_{max}})
of the unscaled covariance matrix is decreasing.

If only one parameter is dealt with, the unscaled posterior standard
error is given.

There is no critical value under which \eqn{\lambda_{max}} is good enough.
By default, 0.1 is used (see \code{getOption("dclone.crit")}).
}
\value{
Returns a single vaue, the maximum of the eigenvalues of the 
unscaled variance covariance matrix of the estimated parameters.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
Khurram Nadeem, \email{knadeem@math.ualberta.ca}

\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Eigen decomposition: \code{\link{eigen}}

This test is used in the function \code{\link{jags.dclone}}
}
\examples{
data(regmod)
lambdamax.diag(regmod)
}
\keyword{ htest }
