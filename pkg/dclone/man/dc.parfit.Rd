\name{dc.parfit}
\Rdversion{1.1}
\alias{dc.parfit}
\title{
Parallel model fitting with data cloning
}
\description{
The iterative model fitting in on parallel workers with different number of clones.
}
\usage{
dc.parfit(cl, data, params, model, inits, n.clones, 
multiply=NULL, unchanged=NULL, ...)
}
\arguments{
  \item{cl}{
A cluster object created by \code{\link[snow]{makeCluster}}.
}
  \item{data}{
A named list (not environment) containing the data.
}
  \item{params}{
Character vector of parameters to be sampled.
}
  \item{model}{
Character string (name of the model file) or a function containing the model (see Examples).
}
  \item{inits}{
Optional specification of initial values in the form of a list or a function (see Initialization at \code{\link[rjags]{jags.model}}). 
If missing, will be treated as \code{NULL} and initial values will be generated automatically.
}
  \item{n.clones}{
An integer vector containing the numbers of clones to use itaratively.
}
  \item{multiply}{
Numeric or character index for list element(s) in the \code{data} argument
to be multiplied by the number of clones instead of repetitions.
}
  \item{unchanged}{
Numeric or character index for list element(s) in the \code{data} argument
to be left unchanged.
}
  \item{\dots}{
  Other values supplied to \code{\link[dclone]{jags.fit}}, or \code{\link[dclone]{bugs.fit}}, depending on the \code{flavour} argument. (Note, \code{falvour = "bugs"} has not been tested!)
}
}
\details{
The \code{dc.parfit} is a parallel computing version of \code{\link[dclone]{dc.fit}}.
The vector \code{n.clones} is used to determine size balancing. If load balancing is also desired 
besides of size balancing (e.g. due to unequal performance of the workers, the option \code{"dcpar.LB"} should be set to \code{TRUE} (by using \code{options("dcpar.LB" = TRUE)}). By default, the \code{"dcpar.LB"}
option is \code{FALSE} for reproducibility reasons.
}
\value{
An object inheriting from the class 'mcmc.list'.
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\seealso{
Sequential version: \code{\link[dclone]{dc.fit}}.

Optimizing the number of workers: \code{\link{clusterSize}}, \code{\link{plotClusterSize}}.
}
\examples{
\dontrun{
library(dcpar)
set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)
beta <- c(2, -1)
mu <- X \%*\% beta
Y <- rpois(n, exp(mu))
glm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}
dat <- list(Y=Y, X=X, n=n, np=ncol(X))
k <- 1:3
cl <- makeCluster(2, type = "SOCK")
dcm <- dc.fit(dat, "beta", glm.model, n.clones=k, multiply="n", unchanged="np")
pdcm <- dc.parfit(cl, dat, "beta", glm.model, n.clones=k, multiply="n", unchanged="np")
summary(dcm)
summary(pdcm)
stopCluster(cl)
}
}
\keyword{ models }
\keyword{ htest }
