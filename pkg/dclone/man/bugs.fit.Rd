\encoding{UTF-8}
\name{bugs.fit}
\Rdversion{1.1}
\alias{bugs.fit}
\alias{as.mcmc.list.bugs}
\title{
Fit BUGS models with cloned data
}
\description{
Convenient functions designed to work well with cloned data arguments
and WinBUGS and OpenBUGS.
}
\usage{
bugs.fit(data, params, model, inits = NULL, 
format = c("mcmc.list", "bugs"), program = c("winbugs", "openbugs"), ...)
\method{as.mcmc.list}{bugs}(x, ...)
}
\arguments{
  \item{data}{
A list containing the data.
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
  \item{format}{
Required output format.
}
  \item{program}{
The program to use, not case sensitive.
\code{winbugs} calls the function \code{\link[R2WinBUGS]{bugs}},
\code{openbugs} calls the function \code{\link[R2WinBUGS]{openbugs}} and requires the CRAN package \pkg{BRugs}.
}
  \item{x}{
A fitted 'bugs' object.
}
  \item{\dots}{
Further arguments of the \code{\link[R2WinBUGS]{bugs}} function,
except for \code{codaPkg} are passed also, most notably the ones to set up burn-in, thin, etc. (see Details).
}
}
\value{
By default, an \code{mcmc.list} object. If data cloning is used via the 
\code{data} argument, \code{summary} returns a modified summary
containing scaled data cloning standard errors (scaled by \code{sqrt(n.clones)}),
and \eqn{R_{hat}} values (as returned by \code{\link[coda]{gelman.diag}}).

\code{bugs.fit} can return a \code{bugs} object if \code{format = "bugs"}.
In this case, printing is not changed, but the number of clones used is attached as attribute
and can be retrieved by the function \code{\link{nclones}}.

The function \code{as.mcmc.list.bugs} converts a 'bugs' object into 'mcmc.list' and retrieves
data cloning information as well.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Underlying functions: \code{\link[R2WinBUGS]{openbugs}}, \code{\link[R2WinBUGS]{bugs}}

Methods: \code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}}, 
\code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}}, \code{\link{vcov.mcmc.list.dc}}
}
\examples{
\dontrun{
## fitting with WinBUGS, bugs example
data(schools)
dat <- list(J = nrow(schools), y = schools$estimate, sigma.y = schools$sd)
bugs.model <- function(){
       for (j in 1:J){
         y[j] ~ dnorm (theta[j], tau.y[j])
         theta[j] ~ dnorm (mu.theta, tau.theta)
         tau.y[j] <- pow(sigma.y[j], -2)
       }
       mu.theta ~ dnorm (0.0, 1.0E-6)
       tau.theta <- pow(sigma.theta, -2)
       sigma.theta ~ dunif (0, 1000)
     }  
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
sim <- bugs.fit(dat, param, bugs.model, inits)
dat2 <- dclone(dat, 2, multiply="J")
sim2 <- bugs.fit(dat2, param, bugs.model)
## fitting the model with OpenBUGS
sim3 <- bugs.fit(dat2, param, bugs.model, program="openbugs", n.thin=1)
## fitting the model with JAGS
sim4 <- jags.fit(dat2, param, bugs.model)
}
}
\keyword{ models }
\keyword{ htest }
