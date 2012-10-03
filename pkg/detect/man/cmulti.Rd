\name{cmulti}
\alias{cmulti}
\alias{cmulti.fit}
\title{
Conditional Multinomial Maximum Likelihood Estimation
}
\description{
Conditional Multinomial Maximum Likelihood Estimation
for different sampling methodologies.
}
\usage{
cmulti(formula, data, type = c("rem", "mix", "dis"), 
inits = NULL, method = "Nelder-Mead", ...)
cmulti.fit(Y, D, X=NULL, type=c("rem", "mix", "dis"), 
inits=NULL, method="Nelder-Mead", ...)
}
\arguments{
  \item{formula}{
formula, LHS takes 2 matrices in the form of \code{Y | D}, 
RHS is either \code{1} or some covariates, see Examples.
}
  \item{data}{
data.
}
  \item{type}{
character, one of \code{"rem"} (removal sampling, homogenerous 
singing rates), \code{"mix"} (removal sampling, heterogenerous 
singing rates), \code{"dis"} (distance sampling, half-normal detection 
function for point counts, circular area)
}
  \item{Y, D, X}{
design matrices, \code{Y} contains counts, \code{D} refers to
sampling methodology, \code{X} is the matrix with covariates.
}
  \item{inits}{
optional initial values.
}
  \item{method}{
method for \code{\link{optim}}.
}
  \item{\dots}{
additional options for \code{\link{optim}}.
}
}
\details{
Conditional Multinomial Maximum Likelihood Estimation
for different sampling methodologies.
}
\value{
An object of class 'cmulti'.
}
\references{
Solymos et al.
}
\author{
Peter Solymos
}
\examples{
simfun1 <- function(n = 10, phi = 0.1, c=1, sigma=0.8, type="rem") {
    if (type=="dis") {
        Dparts <- matrix(c(0.5, 1, NA,
                      0.5, 1, Inf,
                      1, Inf, NA), 3, 3, byrow=TRUE)
        D <- Dparts[sample.int(3, n, replace=TRUE),]
        CP <- 1-exp(-(D/sigma)^2)
    } else {
        Dparts <- matrix(c(5, 10, NA,
                      3, 5, 10,
                      3, 5, NA), 3, 3, byrow=TRUE)
        D <- Dparts[sample.int(3, n, replace=TRUE),]
        CP <- 1-c*exp(-D*phi)
    }
    k <- ncol(D)
    P <- CP - cbind(0, CP[, -k, drop=FALSE])
    Psum <- rowSums(P, na.rm=TRUE)
    PPsum <- P / Psum
    Pok <- !is.na(PPsum)
    N <- rpois(n, 10)
    Y <- matrix(NA, ncol(PPsum), nrow(PPsum))
    Ypre <- sapply(1:n, function(i) rmultinom(1, N, PPsum[i,Pok[i,]]))
    Y[t(Pok)] <- unlist(Ypre)
    Y <- t(Y)
    list(Y=Y, D=D)
}

n <- 1000
x <- rnorm(n)
X <- cbind(1, x)

## removal, constant
vv <- simfun1(n=n, phi=exp(-1.5))
m1 <- cmulti(vv$Y | vv$D ~ 1, type="rem")
coef(m1)
## mixture, constant
vv <- simfun1(n=n, phi=exp(-1.5), c=plogis(0.8))
m2 <- cmulti(vv$Y | vv$D ~ 1, type="mix")
coef(m2)
## dist, constant
vv <- simfun1(n=n, sigma=exp(-0.2), type="dis")
m3 <- cmulti(vv$Y | vv$D ~ 1, type="dis")
coef(m3)

## removal, not constant
log.phi <- X \%*\% c(-2,-1)
vv <- simfun1(n=n, phi=exp(cbind(log.phi, log.phi, log.phi)))
m1 <- cmulti(vv$Y | vv$D ~ x, type="rem")
coef(m1)
## mixture, not constant
logit.c <- X \%*\% c(-2,1)
vv <- simfun1(n=n, phi=exp(-1.5), c=plogis(cbind(logit.c, logit.c, logit.c)))
m2 <- cmulti(vv$Y | vv$D ~ x, type="mix")
coef(m2)
## dist, not constant
log.sig <- X \%*\% c(-0.5,-0.2)
vv <- simfun1(n=n, sigma=exp(cbind(log.sig, log.sig, log.sig)), type="dis")
m3 <- cmulti(vv$Y | vv$D ~ x, type="dis")
coef(m3)

summary(m3)
coef(m3)
vcov(m3)
AIC(m3)
confint(m3)
logLik(m3)
}
\keyword{ htest }
