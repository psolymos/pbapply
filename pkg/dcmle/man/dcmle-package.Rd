\name{dcmle-package}
\alias{dcmle-package}
\docType{package}
\title{
Hierarchical Models Made Easy with Data Cloning
}
\description{
S4 classes around infrastructure
provided by the dclone package to make package 
development with data cloning for hierarchical 
models easy as a breeze.
}
\details{
The package defines S4 object classes for plain BUGS models 
(\code{"\linkS4class{gsFit}"}, after BU*GS*/JA*GS*),
and BUGS models made ready for data cloning (\code{"\linkS4class{dcFit}"}).
It also defines virtual classes for S3 object classes defined in
the \pkg{dclone} and \pkg{coda} packages. 

The S4 class \code{"\linkS4class{dcmle}"} is a fitted 
model object containing MCMC results as returned by the
\code{\link{dcmle}} function. 
These object classes are easily
extensible to allow inclusion into functions fitting
specific models to the data (see Examples).

\code{\link{sourceDcExample}} can be used to get
examples from \url{http://dcr.r-forge.r-project.org/examples}
gathered from various sources (WinBUGS Manual, articles, books, etc.).
This under continuous development, and used in the testing suite
of the \pkg{dclone} package. Contributions of examples are welcome.
}
\author{
Peter Solymos

Maintainer: Peter Solymos <solymos@ualberta.ca>
}
\references{
\url{http://cran.r-project.org/package=dcmle}, 
\url{http://dcr.r-forge.r-project.org}
}
\keyword{ package }
\seealso{
Fitting wrapper function: \code{\link{dcmle}}

Object classes: \code{"\linkS4class{dcmle}"},
\code{"\linkS4class{codaMCMC}"}, \code{"\linkS4class{dcCodaMCMC}"}

Creator functions \code{\link{makeGsFit}} and 
\code{\link{makeDcFit}}
}
\examples{
## Data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.

## Function to create template object for the Beverton-Holt model
## R CMD check will not choke on character representation of model
## the convenient makeDcFit creator function is used here
bevholtFit <- function(y) {
    makeDcFit(data = list(ncl=1, n=length(y), Y=dcdim(data.matrix(y))),
        model = structure(
            c(" model { ",
            "         for (k in 1:ncl) { ",
            "             for(i in 2:(n+1)){ ",
            "                 Y[(i-1), k] ~ dpois(exp(X[i, k])) ",
            "                 X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2) ",
            "                 mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k])) ",
            "             } ",
            "             X[1, k] ~ dnorm(mu0, 1 / sigma^2) ",
            "         } ",
            "         beta ~ dlnorm(-1, 1) ",
            "         sigma ~ dlnorm(0, 1) ",
            "         tmp ~ dlnorm(0, 1) ",
            "         lambda <- tmp + 1 ",
            "         mu0 <- log(2)  + log(lambda) - log(1 + beta * 2) ",
            "     } "),
            class = "custommodel"),
        multiply = "ncl",
        unchanged = "n",
        params <- c("lambda","beta","sigma"))
}
## S4 class 'bevholtMle' extends the 'dcMle' class
## it can have additional slots
setClass("bevholtMle", 
    representation(y="numeric", title="character"), 
    contains = "dcmle")
## Function to fit the Beverton-Holt model to data
bevholt <- function(y, n.clones, ...) {
    new("bevholtMle",
        dcmle(bevholtFit(y), n.clones=n.clones, ...),
        y = y,
        title = "Beverton-Holt Model")
}
## Show method with appropriate heading
setMethod("show", "bevholtMle", function(object)
    show(summary(as(object, "dcmle"), object@title)))
paurelia <- c(17,29,39,63,185,258,267,392,510,
    570,650,560,575,650,550,480,520,500)
\dontrun{
(m <- bevholt(paurelia, n.clones=2, n.iter=1000))
vcov(m)
m@y
}
}
