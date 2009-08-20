\encoding{UTF-8}
\name{jags.fit}
\Rdversion{1.1}
\alias{jags.fit}
\alias{summary.mcmc.list.dc}
\alias{print.summary.mcmc.dc}
\title{
Convenient function to fit JAGS models.
}
\description{
Function to fit a JAGS model in just one line.
}
\usage{
jags.fit(data, params, model, n.chains = 3, n.adapt = 1000,
n.update = 0, thin = 1, n.iter = 5000, progress.bar = "text", ...)
\method{summary}{mcmc.list.dc}(object, 
quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...)
\method{print}{summary.mcmc.dc}(x, 
digits = max(3, .Options$digits - 3), ...)
}
\arguments{
  \item{data}{
A list containing the data.
}
  \item{params}{
Character vector of parameters to be samples.
}
  \item{model}{
Character string (name of the model file) or a function containing the model (see Examples).
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
  \item{progress.bar}{
Type of progress bar (see \code{\link[rjags]{update.jags}}).
}
  \item{quantiles}{
  Quantiles to be shown in the summary of the MCMC object.
}
  \item{digits}{
  Number of significant digits shown in the summary of the MCMC object.
}
  \item{x, object}{
  MCMC object to be printed or summarized.
}
  \item{\dots}{
Further arguments passed to \code{\link[rjags]{jags.model}}, 
e.g. the \code{inits} argument.
}
}
\value{
An \code{mcmc} or \code{mcmc.list} object. If data cloning is used via the 
\code{data} argument, \code{summary(jags.fit)} returns a modified summary
containing scaled data cloning standard errors (scaled by \code{sqrt(n.clones)}),
and \eqn{R_{hat}} values (as returned by \code{\link[rjags]{gelman.diag}}).
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[rjags]{jags.model}}, \code{\link[coda]{coda.samples}}
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
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## fit the model
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

