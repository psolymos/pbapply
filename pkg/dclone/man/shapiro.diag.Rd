\name{shapiro.diag}
\Rdversion{1.1}
\alias{shapiro.diag}
\title{
Shapiro-Wilk Multivariate Normality Test for MCMC Objects
}
\description{
Calculates Shapiro-Wilk Multivariate Normality Test for MCMC Objects.
}
\usage{
shapiro.diag(x)
}
\arguments{
  \item{x}{
  An object of class \code{mcmc} or \code{mcmc.list}.
}
}
\details{
This diagnostics can be used to test for the data cloning convergence.
Asymptotically the posterior distribution of the parameters approaches
a degenerate multivariate normal distribution. 

If the posterior
distribution of the parameters is multivariate normal, the p-value
of the test should be higher than a pre defined critical value.
The critical value above which multivariate normality is reached
is default set as 0.1 (see \code{getOption("dclone.crit")}).

This test is also useful for testing identifyability problems.
If parameters are non-identifyable, multivariate normality
will not occur even for high number of clones.

The underlying function (\code{\link[mvnormtest]{mshapiro.test}},
and \code{\link{shapiro.test}})
can handle only 5000 observations, thus in most of the cases
(when number of chains and iterations exceed 5000) only the last 
iterations of the MCMC objects are used.
}
\value{
Returns an object of class \code{htest}.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

See references for the test and the algoritmn in the help page of
\code{\link{shapiro.test}} and \code{\link[mvnormtest]{mshapiro.test}}
}
\author{
Khurram Nadeem, \email{knadeem@math.ualberta.ca}

\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
The test used in this function: \code{\link[mvnormtest]{mshapiro.test}}

This diagnostic test is used in the function \code{\link{jags.dclone}}
}
\examples{
data(regmod)
shapiro.diag(regmod)
}
\keyword{ htest }
