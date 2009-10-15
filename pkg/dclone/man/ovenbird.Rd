\encoding{UTF-8}
\name{ovenbird}
\Rdversion{1.1}
\alias{ovenbird}
\docType{data}
\title{
Abundances of ovenbird in Alberta
}
\description{
The data set contains observations of 310 sites of the Alberta Biodiversity Monitoring Institute.

\code{count}: integer, ovenbird counts per site.

\code{ecosite}: factor with 5 levels, ecological categorisation of the sites.

\code{disturb.succ, disturb.alien}: numeric, percentage of successional and alienating human disturbance
within 500 m radius of the site location.

\code{long, lat}: numeric, public longitute/latitude coordinates of the sites.
}
\usage{data(coralheads)}
\source{
Alberta Biodiversity Monitoring Institute, \url{http://www.abmi.ca}
}
\examples{
data(ovenbird)
summary(ovenbird)
str(ovenbird)
\dontrun{
## jags.dclone example
data(ovenbird)
obform <- count ~ ecosite + disturb.succ + disturb.alien + long * lat
X <- model.matrix(obform, ovenbird)
jfun1v <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, tau)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    tau ~ dgamma(priors[(np+1),2], priors[(np+1),1])
    sigma <- 1.0/sqrt(tau)
}
updfun <- function(x) {
    if (missing(x)) {
        par <- coef(glm(obform, ovenbird, family=poisson))
        return(cbind(c(par, 0.001), rep(0.001, length(par) + 1)))
    } else {
        return(cbind(coef(x), rep(0.001, nvar(x))))
    }
}
dat <- list(Y = ovenbird$count, X = X,
    n = nrow(ovenbird), np = ncol(X),
    priors = updfun())
ncl <- c(1, 2)
n.adapt <- 2000
n.update <- 3000
n.iter <- 5000
mod <- jags.dclone(dat, c("beta", "sigma"), jfun1v, 
    n.clones = ncl, multiply = "n", unchanged = "np",
    update = "priors", updatefun = updfun,
    n.adapt = n.adapt, n.update = n.update, n.iter = n.iter)
summary(mod)
}
}
\keyword{datasets}
