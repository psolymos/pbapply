\encoding{UTF-8}
\name{jags.fit}
\Rdversion{1.1}
\alias{jags.fit}
\title{
Fit JAGS models with cloned data
}
\description{
Convenient functions designed to work well with cloned data arguments and JAGS.
}
\usage{
jags.fit(data, params, model, inits = NULL, n.chains = 3, n.adapt = 1000,
n.update = 0, thin = 1, n.iter = 5000, updated.model=TRUE, ...)
}
\arguments{
  \item{data}{
A named list or environment containing the data. If an environment, data cloning might not work properly (has to be done manually).
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function containing the model, or a or \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a function (see Initialization at \code{\link[rjags]{jags.model}}). 
If \code{NULL}, initial values will be generated automatically. It is an error to supply an initial value for an observed node.
}
  \item{n.chains}{
Number of chains to generate.
}
  \item{n.adapt}{
Number of steps for adaptation.
}
  \item{n.update}{
Number of updates before iterations.
}
  \item{thin}{
Thinning value.
}
  \item{n.iter}{
Number of iterations.
}
  \item{updated.model}{
Logical, if the updated model should be attached as attribute (this can be used to further update if convergence
was not satisfactory, see \code{\link{updated.model}} and \code{\link{update.mcmc.list}}).
}
  \item{\dots}{
Further arguments passed to \code{\link[rjags]{coda.samples}}, and \code{\link[rjags]{update.jags}}
(e.g. the \code{progress.bar} argument).
}
}
\value{
An \code{mcmc.list} object. If data cloning is used via the 
\code{data} argument, \code{summary} returns a modified summary
containing scaled data cloning standard errors (scaled by \code{sqrt(n.clones)}),
and \eqn{R_{hat}} values (as returned by \code{\link[coda]{gelman.diag}}).
}
\author{
\enc{Péter Sólymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Underlying functions: \code{\link[rjags]{jags.model}}, \code{\link[rjags]{coda.samples}}

Parallel chain computations: \code{\link{jags.parfit}}

Methods: \code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}}, 
\code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}}, \code{\link{vcov.mcmc.list.dc}}
}
\examples{
\dontrun{
## simple regression example from the JAGS manual
jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x[])
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
linpred <- model.matrix(~x) \%*\% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## fit the model with JAGS
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
## model summary
summary(regmod)
## data cloning
dcdata <- dclone(jdata, 5, multiply = "N")
dcmod <- jags.fit(dcdata, jpara, jfun, n.chains = 3)
summary(dcmod)
}
}
\keyword{ models }
\keyword{ htest }
