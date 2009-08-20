\encoding{UTF-8}
\name{clean.jags.model}
\Rdversion{1.1}
\alias{write.jags.model}
\alias{clean.jags.model}
\title{
Remove JAGS model from hard drive
}
\description{
Removes a (JAGS model) file from the hard drive.
}
\usage{
write.jags.model(model, filename = "model.bug", dir = getwd(), n = 5)
clean.jags.model(filename = "model.bug", dir = getwd())
}
\arguments{
  \item{model}{
JAGS model to write onto the hard drive (see Example).
}
  \item{filename}{
Character, the name of the file to write/remove.
}
  \item{dir}{
Optional argument for directory where to write or 
look for the file to remove.
}
  \item{n}{
Numeric, length of a random string to be attached to \code{filename} 
if \code{filename} already exists.
}
}
\details{
\code{write.jags.model} is built upon the function 
\code{\link[R2WinBUGS]{write.model}} of the \pkg{R2WinBUGS} package.

\code{clean.jags.model} is built upon the function 
\code{\link{file.remove}}, and 
intended to be used internally to clean the JAGS 
model file up after estimating sessions, 
ideally via the \code{\link{on.exit}} function.
}
\value{
\code{write.jags.model} invisibly returns the name of the file
that was written eventually (possibly including random string).

\code{clean.jags.model} invisibly returns the result of 
\code{\link{file.remove}} (logical).
Original working directory is then restored.
}
\author{
\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
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
linpred <- model.matrix(~x) %*% c(alpha, beta)
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
}
\keyword{ IO }
