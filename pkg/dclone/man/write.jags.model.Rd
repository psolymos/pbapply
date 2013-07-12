\encoding{UTF-8}
\name{write.jags.model}
\Rdversion{1.1}
\alias{write.jags.model}
\alias{clean.jags.model}
\alias{custommodel}
\title{
Write and remove model file
}
\description{
Writes or removes a BUGS model file to or from the hard drive.
}
\usage{
write.jags.model(model, filename = "model.txt", digits = 5,
    dir = getwd(), overwrite = getOption("dcoptions")$overwrite)
clean.jags.model(filename = "model.bug", dir = getwd())
custommodel(model, exclude = NULL, digits = 5)
}
\arguments{
  \item{model}{
JAGS model to write onto the hard drive (see Example).
For \code{write.jags.model}, it can be name of a file or a function,
or it can be an 'custommodel' object returned by \code{custommodel}.
\code{custommodel} can take its \code{model} argument as function. 
If \code{model} is not function, its is coerced as character.
}
  \item{digits}{
Number of significant digits used in the output.
}
  \item{filename}{
Character, the name of the file to write/remove.
It can be a \code{link{connection}}.
}
  \item{dir}{
Optional argument for directory where to write or 
look for the file to remove.
}
  \item{overwrite}{
Logical, if \code{TRUE} the \code{filename} will be forced and existing
file with same name will be overwritten.
}
  \item{exclude}{
Numeric, lines of the model to exclude (see Details).
}
}
\details{
\code{write.jags.model} is built upon the function 
\code{\link[R2WinBUGS]{write.model}} of the \pkg{R2WinBUGS} package.

\code{clean.jags.model} is built upon the function 
\code{\link{file.remove}}, and 
intended to be used internally to clean up the JAGS 
model file after estimating sessions, 
ideally via the \code{\link{on.exit}} function.

The function \code{custommodel} can be used to exclude some lines
of the model. This is handy when there are variations of the same model.
\code{write.jags.model} accepts results returned by \code{custommodel}.
This is also the preferred way of including BUGS models into
R packages, because the function form often includes 
undefined functions.

Use the \code{\%_\%} operator if the model is a function and the model
contains truncation (\code{I()} in WinBUGS, \code{T()} in JAGS).
See explanation on help page of \code{\link[R2WinBUGS]{write.model}}.
}
\value{
\code{write.jags.model} invisibly returns the name of the file
that was written eventually (possibly including random string).

\code{clean.jags.model} invisibly returns the result of 
\code{\link{file.remove}} (logical).
Original working directory is restored.

\code{custommodel} returns an object of class 'custommodel', 
which is a character vector.
}
\author{
Peter Solymos, \email{solymos@ualberta.ca}
}
\seealso{
\code{\link[R2WinBUGS]{write.model}}, \code{\link{file.remove}}
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
linpred <- model.matrix(~x) \%*\% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "sigma")
## write model onto hard drive
jmodnam <- write.jags.model(jfun)
## fit the model
regmod <- jags.fit(jdata, jpara, jmodnam, n.chains = 3)
## cleanup
clean.jags.model(jmodnam)
## model summary
summary(regmod)
}
## let's customize this model
jfun2 <- structure(
    c(" model { ",
    "     for (i in 1:n) { ",
    "         Y[i] ~ dpois(lambda[i]) ",
    "         Y[i] <- alpha[i] + inprod(X[i,], beta[1,]) ",
    "         log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,]) ",
    "         alpha[i] ~ dnorm(0, 1/sigma^2) ",
    "     } ",
    "     for (j in 1:np) { ",
    "         beta[1,j] ~ dnorm(0, 0.001) ",
    "     } ",
    "     sigma ~ dlnorm(0, 0.001) ",
    " } "),
    class = "custommodel")
custommodel(jfun2)
## GLMM
custommodel(jfun2, 4)
## LM
custommodel(jfun2, c(3,5))
## deparse when print
print(custommodel(jfun2), deparse=TRUE)
}
\keyword{ IO }
