\encoding{UTF-8}
\name{bugs.parfit}
\Rdversion{1.1}
\alias{bugs.parfit}
\title{
Parallel computing with WinBUGS/OpenBUGS
}
\description{
Does the same job as \code{\link[dclone]{bugs.fit}}, 
but parallel chanis are run on parallel workers, thus 
computations can be faster (up to 1/\code{n.chains}) for long MCMC runs.
}
\usage{
bugs.parfit(cl, data, params, model, inits=NULL, n.chains = 3, 
    seed, program=c("winbugs", "openbugs", "brugs"), ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}, or
an integer, see \code{\link{parDosa}} and 
\code{\link{evalParallelArgument}}.
}
  \item{data}{
A named list or environment containing the data. If an environment, 
\code{data} is coerced into a list.
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file), a function 
containing the model, or a or \code{\link{custommodel}} 
object (see Examples).
}
  \item{inits}{
Specification of initial values in the form of a 
list or a function, can be missing.
If this is a function and using 'snow' type
cluster as \code{cl}, the function must be self containing, i.e. 
not having references to R objects outside of the
function, or the objects should be exported with 
\code{\link{clusterExport}} before calling \code{bugs.parfit}.
Forking type parallelism does not require such attention.
}
  \item{n.chains}{
Number of chains to generate, must be higher than 1. Ideally, 
this is equal to the number of parallel workers in the cluster.
}
  \item{seed}{
Vector of random seeds, must have \code{n.chains} unique values.
See Details.
}
  \item{...}{
Other arguments passed to \code{\link[dclone]{bugs.fit}}.
}
}
\details{
Chains are run on parallel workers, 
and the results are combined in the end.

The seed must be supplied, as it is the user's responsibility
to make sure that pseudo random sequences do not seriously
overlap. 

The WinBUGS implementation is quite unsafe from this regard,
because the pseudo-random number generator used by WinBUGS 
generates a finite (albeit very long) sequence of distinct numbers, 
which would eventually be repeated if the sampler 
were run for a sufficiently long time.
Thus it's usage must be discouraged. That is the reason for the
warning that is issued when \code{program = "winbugs"}.

OpenBUGS (starting from version 3.2.2) implemented a system
where internal state of the pseudo random number generator can be 
set to one of 14 predefined states (seed values in \code{1:14}).
Each predefined state is 10^12 draws apart to avoid overlap in 
pseudo random number sequences.
}
\value{
An \code{mcmc.list} object.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
Sequential version: \code{\link[dclone]{bugs.fit}}
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
SEED <- floor(runif(3, 10000, 99999))
if (.Platform$OS.type == "windows") {
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=SEED)
summary(sim)
}
dat2 <- dclone(dat, 2, multiply="J")
if (.Platform$OS.type == "windows") {
sim2 <- bugs.parfit(cl, dat2, param, bugs.model, 
    program="winbugs", n.iter=2000, n.thin=1, seed=SEED)
summary(sim2)
}
## fitting the model with OpenBUGS
## using the less preferred BRugs interface
sim3 <- bugs.parfit(cl, dat2, param, bugs.model, 
    program="brugs", n.iter=2000, n.thin=1, seed=1:3)
summary(sim3)
library(R2OpenBUGS)
## fitting the model with OpenBUGS
## using the preferred R2OpenBUGS interface
sim4 <- bugs.parfit(cl, dat2, param, bugs.model, 
    program="openbugs", n.iter=2000, n.thin=1, seed=1:3)
summary(sim4)
}
}
\keyword{ models }
