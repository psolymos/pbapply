\name{dcmle}
\alias{dcmle}
\title{
Asymptotic Maximum Likelihood Estimation with Data Cloning
}
\description{
This function is a wrapper to fit the model to the data
and obtain MLE point estimates and asymptotic standard errors
based on the estimate of the Fisher information matrix
(theory given by Lele et al. 2007, 2010, software
implementation is given in Solymos 2010).
}
\usage{
dcmle(x, params, n.clones = 1, cl = NULL, ...)
}
\arguments{
  \item{x}{
an object of class \code{\link{gsFit}} or \code{\link{dcFit}}.
}
  \item{params}{
character, vector of model parameters to monitor.
}
  \item{n.clones}{
integer, vector for the number of clones used in fitting.
}
  \item{cl}{
cluster object (snow type cluster) 
or number of cores (multicore type forking), optinal.
}
  \item{\dots}{
other arguments passed to underlying functions (see Details).
}
}
\details{
The function uses slots of the input object
and passes them as arguments to underlying functions 
(\code{\link[dclone]{jags.fit}}, \code{\link[dclone]{jags.parfit}}, 
\code{\link[dclone]{bugs.fit}},
\code{\link[dclone]{dc.fit}}, \code{\link[dclone]{dc.parfit}}).
}
\value{
An object of class \code{\link{dcMle}}.
}
\references{
Solymos, P., 2010. dclone: Data Cloning in R.
\emph{The R Journal} \strong{2(2)}, 29--37.
URL: \url{http://journal.r-project.org/archive/2010-2/RJournal_2010-2_Solymos.pdf}

Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized linear 
mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{105}, 1617--1625.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
For additional arguments: \code{\link[dclone]{jags.fit}}, 
\code{\link[dclone]{jags.parfit}}, 
\code{\link[dclone]{bugs.fit}},
\code{\link[dclone]{dc.fit}}, \code{\link[dclone]{dc.parfit}}.

Object classes: \code{\link{dcmle-classes}}
with creator functions \code{\link{makeGsFit}} and 
\code{\link{makeDcFit}}

Methods: \code{\link{dcmle-methods}}
}
\examples{
## Data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,
    570,650,560,575,650,550,480,520,500)
paramecium <- new("dcFit")
paramecium@data <- list(
    ncl=1, 
    n=length(paurelia), 
    Y=dcdim(data.matrix(paurelia)))
paramecium@model <- function() {
    for (k in 1:ncl) {
        for(i in 2:(n+1)){
            Y[(i-1), k] ~ dpois(exp(X[i, k])) # observations
            X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2) # state
            mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k]))
        }
        X[1, k] ~ dnorm(mu0, 1 / sigma^2) # state at t0
    }
    beta ~ dlnorm(-1, 1) # Priors on model parameters
    sigma ~ dlnorm(0, 1)
    tmp ~ dlnorm(0, 1)
    lambda <- tmp + 1
    mu0 <- log(2)  + log(lambda) - log(1 + beta * 2)
}
paramecium@multiply <- "ncl"
paramecium@unchanged <- "n"
paramecium@params <- c("lambda","beta","sigma")
\dontrun{
(m1 <- dcmle(paramecium, n.clones=1, n.iter=1000))
(m2 <- dcmle(paramecium, n.clones=2, n.iter=1000))
(m3 <- dcmle(paramecium, n.clones=1:3, n.iter=1000))
cl <- makePSOCKcluster(3)
(m4 <- dcmle(paramecium, n.clones=2, n.iter=1000, cl=cl))
(m5 <- dcmle(paramecium, n.clones=1:3, n.iter=1000, cl=cl))
(m6 <- dcmle(paramecium, n.clones=1:3, n.iter=1000, cl=cl, 
    partype="parchains"))
(m7 <- dcmle(paramecium, n.clones=1:3, n.iter=1000, cl=cl, 
    partype="both"))
stopCluster(cl)
}
}
\keyword{ models }
\keyword{ htest }
