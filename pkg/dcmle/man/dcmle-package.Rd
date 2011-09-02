\name{dcmle-package}
\alias{dcmle-package}
\docType{package}
\title{
Hierarchical Models Made Easy with Data Cloning
}
\description{
S4 classes around dclone infrastructure
to make package development easy as a breeze.
}
\details{
The package defines S4 object classes for plain BUGS models ('gsFit'),
and BUGS models made ready for data cloning ('dcFit').
It also defines virtual classes for S3 object classes defined in
the \pkg{dclone} package. The S4 class 'dcMle' is a fitted 
model object containing MCMC results as returned by the
\code{\link{dcmle}} function. Methods are defined
for 'dcMle' object class. These object classes are easily
extendable to allow inclusion into functions fitting
specific models to the data.

(Not yet fully implemented: see dcexamples package
and vignette there on how-to.)
}
\author{
Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
\url{http://dcr.r-forge.r-project.org}
}
\keyword{ package }
\seealso{
\code{\link{dcmle}}
}
\examples{
## Function to create the Beverton-Holt model template
## R CMD check will not choke on character representation of model
bevholtFit <- function(y) {
    x <- new("dcFit")
    x@data <- list(ncl=1, n=length(y), Y=dcdim(data.matrix(y)))
    x@model <- structure(
        c(" model { ",
        "         for (k in 1:ncl) { ",
        "             for(i in 2:(n+1)){ ",
        "                 Y[(i-1), k] ~ dpois(exp(X[i, k])) # observations ",
        "                 X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2) # state ",
        "                 mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k])) ",
        "             } ",
        "             X[1, k] ~ dnorm(mu0, 1 / sigma^2) # state at t0 ",
        "         } ",
        "         beta ~ dlnorm(-1, 1) # Priors on model parameters ",
        "         sigma ~ dlnorm(0, 1) ",
        "         tmp ~ dlnorm(0, 1) ",
        "         lambda <- tmp + 1 ",
        "         mu0 <- log(2)  + log(lambda) - log(1 + beta * 2) ",
        "     } "),
        class = "custommodel")
    x@multiply <- "ncl"
    x@unchanged <- "n"
    x@params <- c("lambda","beta","sigma")
    x
}
## S4 class 'bevholtMle' extends the 'dcMle' class
## that can have additional slots
setClass("bevholtMle", 
    representation(y="numeric", title="character"), 
    contains = "dcMle")
## Function to fit the Beverton-Holt model to data
bevholt <- function(y, n.clones, ...) {
    x <- as(dcmle(bevholtFit(y), n.clones=n.clones, ...), "bevholtMle")
    x@y <- y
    x@title <- "Beverton-Holt Model"
    x
}
## Show method with appropriate title
setMethod("show", "bevholtMle", function(object) {
    summary(as(object, "dcMle"), object@title)
})
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
\dontrun{
(m <- bevholt(paurelia, n.clones=2, n.iter=1000))
vcov(m)
m@y
}
}
