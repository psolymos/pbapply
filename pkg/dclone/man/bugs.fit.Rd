\encoding{UTF-8}
\name{bugs.fit}
\Rdversion{1.1}
\alias{bugs.fit}
\alias{as.mcmc.list.bugs}
\title{
Fit BUGS models with cloned data
}
\description{
Convenient functions designed to work well with 
cloned data arguments
and WinBUGS and OpenBUGS.
}
\usage{
bugs.fit(data, params, model, inits = NULL, n.chains = 3,
    format = c("mcmc.list", "bugs"), 
    program = c("winbugs", "openbugs", "brugs"), 
    seed, ...)
\method{as.mcmc.list}{bugs}(x, ...)
}
\arguments{
  \item{data}{
A list (or environment) containing the data.
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function containing 
the model, or a \code{\link{custommodel}} object (see Examples).
}
  \item{inits}{
Optional specification of initial values in the 
form of a list or a function. 
If \code{NULL}, initial values will be 
generated automatically.
}
  \item{n.chains}{
number of Markov chains.
}
  \item{format}{
Required output format.
}
  \item{program}{
The program to use, not case sensitive.
\code{winbugs} calls the function \code{\link[R2WinBUGS]{bugs}} from
package \pkg{R2WinBUGS},
\code{openbugs} calls the function \code{\link[R2OpenBUGS]{bugs}} 
from package \pkg{R2OpenBUGS} (this has changed since \pkg{dclone}
version 1.8-1, this is now the preferred OpenBUGS interface).
\code{brugs} calls the function \code{\link[R2WinBUGS]{openbugs}} 
from package \pkg{R2WinBUGS} and requires the CRAN package \pkg{BRugs}
(this is provided for back compatibility purposes, but gives a warning
because it is not the preferred interface to R2OpenBUGS).
}
  \item{seed}{
Random seed (\code{bugs.seed} argument for 
\code{\link[R2WinBUGS]{bugs}} in package \pkg{R2WinBUGS} or 
\code{\link[R2OpenBUGS]{bugs}} in package \pkg{R2OpenBUGS}, 
\code{seed} argument for \code{\link[R2WinBUGS]{openbugs}}).
It takes the corresponding default values (\code{NULL} or \code{1})
when missing.
}
  \item{x}{
A fitted 'bugs' object.
}
  \item{\dots}{
Further arguments of the \code{\link[R2WinBUGS]{bugs}} function,
except for \code{codaPkg} are passed also, most notably 
the ones to set up burn-in, thin, etc. (see Details).
}
}
\value{
By default, an \code{mcmc.list} object. If data cloning is used via the 
\code{data} argument, \code{summary} returns a modified summary
containing scaled data cloning standard errors 
(scaled by \code{sqrt(n.clones)}), and
\eqn{R_{hat}} values (as returned by \code{\link[coda]{gelman.diag}}).

\code{bugs.fit} can return a \code{bugs} object if 
\code{format = "bugs"}.
In this case, summary is not changed, but the number of clones 
used is attached as attribute
and can be retrieved by the function \code{\link{nclones}}.

The function \code{as.mcmc.list.bugs} converts a 'bugs' object
into 'mcmc.list' and retrieves
data cloning information as well.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
Underlying functions: 
\code{\link[R2WinBUGS]{bugs}} in package \pkg{R2WinBUGS},
\code{\link[R2WinBUGS]{openbugs}} in package \pkg{R2WinBUGS},
\code{\link[R2OpenBUGS]{bugs}} in package \pkg{R2OpenBUGS}

Methods: \code{\link{dcsd}}, \code{\link{confint.mcmc.list.dc}}, 
\code{\link{coef.mcmc.list}}, \code{\link{quantile.mcmc.list}}, 
\code{\link{vcov.mcmc.list.dc}}
}
\examples{
\dontrun{
## fitting with WinBUGS, bugs example
library(R2WinBUGS)
data(schools)
dat <- list(J = nrow(schools), 
    y = schools$estimate, 
    sigma.y = schools$sd)
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
if (.Platform$OS.type == "windows") {
sim <- bugs.fit(dat, param, bugs.model, inits)
summary(sim)
}
dat2 <- dclone(dat, 2, multiply="J")
if (.Platform$OS.type == "windows") {
sim2 <- bugs.fit(dat2, param, bugs.model, 
    program="winbugs", n.iter=2000, n.thin=1)
summary(sim2)
}
## fitting the model with OpenBUGS
## using the less preferred BRugs interface
sim3 <- bugs.fit(dat2, param, bugs.model, 
    program="brugs", n.iter=2000, n.thin=1)
summary(sim3)
library(R2OpenBUGS)
## fitting the model with OpenBUGS
## using the preferred R2OpenBUGS interface
sim4 <- bugs.fit(dat2, param, bugs.model, 
    program="openbugs", n.iter=2000, n.thin=1)
summary(sim4)
## fitting the model with JAGS
sim5 <- jags.fit(dat2, param, bugs.model)
summary(sim5)
}
}
\keyword{ models }
\keyword{ htest }
