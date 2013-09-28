\encoding{UTF-8}
\name{mcmc.list-methods}
\Rdversion{1.1}
\alias{dcsd}
\alias{dcsd.mcmc.list}
\alias{coef.mcmc.list}
\alias{confint.mcmc.list.dc}
\alias{vcov.mcmc.list.dc}
\alias{vcov.mcmc.list}
\alias{quantile.mcmc.list}
\title{
Methods for the 'mcmc.list' class
}
\description{
Methods for 'mcmc.list' objects.
}
\usage{
dcsd(object, ...)
\method{dcsd}{mcmc.list}(object, ...)
\method{coef}{mcmc.list}(object, ...)
\method{confint}{mcmc.list.dc}(object, parm, level = 0.95, ...)
\method{vcov}{mcmc.list}(object, ...)
\method{vcov}{mcmc.list.dc}(object, invfisher = TRUE, ...)
\method{quantile}{mcmc.list}(x, ...)
}
\arguments{
  \item{x, object}{
  MCMC object to be processed.
}
  \item{parm}{
A specification of which parameters are to be given confidence intervals, 
either a vector of numbers or a vector of names. 
If missing, all parameters are considered.
}
  \item{level}{
The confidence level required.
}
  \item{\dots}{
Further arguments passed to functions.
}
  \item{invfisher}{
Logical, if the inverse of the Fisher information matrix 
(\code{TRUE}) should be returned
instead of the variance-covariance matrix of the joint 
posterior distribution (\code{FALSE}).
}
}
\value{
\code{dcsd} returns the data cloning standard errors of a posterior 
MCMC chain calculated as standard deviation times the square root 
of the number of clones.

The \code{coef} method returns mean of the posterior MCMC chains
for the monitored parameters.

The \code{confint} method returns Wald-type confidence intervals 
for the parameters assuming asymptotic normality.

The \code{vcov} method returns the inverse of the Fisher 
information matrix (\code{invfisher = TRUE}) or the covariance matrix 
of the joint posterior distribution (\code{invfisher = FALSE}). 
The \code{invfisher} is valid only for \code{mcmc.list.dc}
(data cloned) objects.

The \code{quantile} method returns quantiles for each variable.
}
\note{
Some functions only available for the 'mcmc.list.dc' class which 
inherits from class 'mcmc.list'.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link{jags.fit}}, \code{\link{bugs.fit}}
}
\examples{
\dontrun{
## simple regression example from the JAGS manual
jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x)
    alpha ~ dnorm(0.0, 1.0E-4)
    beta ~ dnorm(0.0, 1.0E-4)
    sigma <- 1.0/sqrt(tau)
    tau ~ dgamma(1.0E-3, 1.0E-3)
}
## data generation
set.seed(1234)
N <- 100
alpha <- 1
beta <- -1
sigma <- 0.5
x <- runif(N)
linpred <- crossprod(t(model.matrix(~x)), c(alpha, beta))
Y <- rnorm(N, mean = linpred, sd = sigma)
## data for the model
dcdata <- dclone(list(N = N, Y = Y, x = x), 5, multiply = "N")
## data cloning
dcmod <- jags.fit(dcdata, c("alpha", "beta", "sigma"), jfun, 
    n.chains = 3)
summary(dcmod)
coef(dcmod)
dcsd(dcmod)
confint(dcmod)
vcov(dcmod)
vcov(dcmod, invfisher = FALSE)
quantile(dcmod)
}
}
\keyword{ methods }

