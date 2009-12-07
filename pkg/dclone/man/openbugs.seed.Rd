\name{openbugs.seed}
\Rdversion{1.1}
\alias{openbugs.seed}
\title{
Run OpenBUGS from R
}
\description{
Run OpenBUGS from R, a replacement function for \code{\link[R2WinBUGS]{openbugs}} function with \code{bugs.seed} argument.
}
\usage{
openbugs.seed(data, inits, parameters.to.save, model.file = "model.txt", n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/n.sims)), n.sims = 1000, DIC = TRUE, bugs.directory = "c:/Program Files/OpenBUGS/", working.directory = NULL, digits = 5, bugs.seed = NULL)
}
\arguments{
  \item{data, inits, parameters.to.save, model.file, n.chains, n.iter, n.burnin, n.thin, n.sims, DIC,
    bugs.directory, working.directory, digits}{
Arguments same as in \code{\link[R2WinBUGS]{openbugs}}.
}
  \item{bugs.seed}{
Random seed for OpenBUGS (default is no seed).
}
}
\value{
A 'bugs' object.
}
\seealso{
\code{\link[R2WinBUGS]{openbugs}}
}
\keyword{ htest }
