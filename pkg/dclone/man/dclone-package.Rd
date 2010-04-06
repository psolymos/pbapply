\encoding{UTF-8}
\name{dclone-package}
\Rdversion{1.1}
\alias{dclone-package}
\docType{package}
\title{
Data Cloning
}
\description{
Low level functions for implementing
    maximum likelihood estimating procedures for 
    complex models using data cloning (Lele et al. 2007) and Bayesian
    Markov chain Monte Carlo methods with support 
    for JAGS, WinBUGS and OpenBUGS.
    Parallel computing with JAGS is supported.

Main functions include:

\code{\link{dclone}}, \code{\link{dcdim}}: cloning R objects in various ways.

\code{\link{jags.fit}}, \code{\link{bugs.fit}}: conveniently fit BUGS models.
  (\code{\link{jags.parfit}} fits chains on parallel workers.)

\code{\link{dc.fit}}: iterative model fitting by the data cloning algorithm.
  (\code{\link{dc.parfit}} is the parallelized version.)

\code{\link{dctable}}, \code{\link{dcdiag}}: helps evaluating data cloning 
convergence by descriptive statistics and diagnostic tools.
}
\author{
Authors: \enc{P\'eter S\'olymos}{Peter Solymos}

Maintainer: \enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\references{
Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.
}
\keyword{ package }
