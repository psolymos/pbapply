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
    complex models using data cloning and Bayesian
    Markov chain Monte Carlo methods. 
    Sequential and parallel MCMC support 
    for JAGS, WinBUGS and OpenBUGS.

Main functions include:

\itemize{
  \item{
    \code{\link{dclone}}, \code{\link{dcdim}}, \code{\link{dciid}},
    \code{\link{dctr}}: 
    cloning R objects in various ways.
  }

  \item{
    \code{\link{jags.fit}}, \code{\link{bugs.fit}}: 
    conveniently fit BUGS models.
    \code{\link{jags.parfit}} and \code{\link{bugs.parfit}} fits 
    chains on parallel workers.
  }

  \item{
    \code{\link{dc.fit}}: iterative model fitting by 
    the data cloning algorithm.
    \code{\link{dc.parfit}} is the parallelized version.
  }

  \item{
    \code{\link{dctable}}, \code{\link{dcdiag}}: 
    helps evaluating data cloning 
    convergence by descriptive statistics and diagnostic tools.
    (These are based on e.g. \code{\link{chisq.diag}} 
    and \code{\link{lambdamax.diag}}.)
  }

  \item{
    \code{\link{coef.mcmc.list}}, \code{\link{confint.mcmc.list.dc}},
    \code{\link{dcsd.mcmc.list}}, \code{\link{quantile.mcmc.list}},
    \code{\link{vcov.mcmc.list.dc}}, \code{\link{mcmcapply}},
    \code{\link{stack.mcmc.list}}: 
    methods for \code{mcmc.list} objects.
  }

  \item{
    \code{\link{write.jags.model}}, \code{\link{clean.jags.model}},
    \code{\link{custommodel}}: 
    convenient functions for handling \code{BUGS} models.
  }

  \item{
    \code{\link{jagsModel}}, \code{\link{codaSamples}}: basic functions
    from \pkg{rjags} package rewrote to recognize data cloning
    attributes from data (\code{\link{parJagsModel}}, 
    \code{\link{parUpdate}}, \code{\link{parCodaSamples}} 
    are the parallel versions).
  }
}
}
\author{
Author: Peter Solymos

Maintainer: Peter Solymos, \email{solymos@ualberta.ca}
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
Estimability and likelihood inference for generalized 
linear mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{105}, 1617--1625.
}
\keyword{ package }
