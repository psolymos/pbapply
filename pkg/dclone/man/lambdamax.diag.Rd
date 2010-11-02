\name{lambdamax.diag}
\Rdversion{1.1}
\alias{lambdamax.diag}
\alias{chisq.diag}
\title{
Maximum Eigenvalue of the Posterior Variance-Covariance Matrix
}
\description{
Calculates the maximum eigenvalue of the posterior variance-covariance matrix
}
\usage{
lambdamax.diag(x)
chisq.diag(x)
}
\arguments{
  \item{x}{
  An object of class \code{mcmc} or \code{mcmc.list}.
}
}
\details{
This diagnostics can be used to test for the data cloning convergence
(Lele et al. 2007, 2010).
Asymptotically the posterior distribution of the parameters approaches
a degenerate multivariate normal distribution. As the distribution
is getting more degenerate, the maximal eigenvalue (\eqn{\lambda_{max}})
of the unscaled covariance matrix is decreasing.

If only one parameter is dealt with, the unscaled posterior standard
error is given.

There is no critical value under which \eqn{\lambda_{max}} is good enough.
By default, 0.05 is used (see \code{getOption("dclone")$diag}).
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

Lele, S.R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized linear
mixed models using data cloning. 
\emph{Journal of the American Statistical Association}
In press.
}
\author{
Khurram Nadeem, \email{knadeem@math.ualberta.ca}

\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Eigen decomposition: \code{\link{eigen}}
}
\examples{
data(regmod)
lambdamax.diag(regmod)
chisq.diag(regmod)
}
\keyword{ htest }
