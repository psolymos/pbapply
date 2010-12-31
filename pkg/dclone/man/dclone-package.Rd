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
  (\code{\link{jags.parfit}} fits chains on parallel workers for JAGS.)

\code{\link{dc.fit}}: iterative model fitting by the data cloning algorithm.
  (\code{\link{dc.parfit}} is the parallelized version.)

\code{\link{dctable}}, \code{\link{dcdiag}}: helps evaluating data cloning 
  convergence by descriptive statistics and diagnostic tools.
  (These are based on e.g. \code{\link{chisq.diag}} 
  and \code{\link{lambdamax.diag}}.)

\code{\link{coef.mcmc.list}}, \code{\link{confint.mcmc.list.dc}},
  \code{\link{dcsd.mcmc.list}}, \code{\link{quantile.mcmc.list}},
  \code{\link{vcov.mcmc.list.dc}},   \code{\link{mcmcapply}}: 
  convenient functions for \code{mcmc.list} objects.

\code{\link{write.jags.model}}, \code{\link{clean.jags.model}},
  \code{\link{custommodel}}: 
  convenient functions for handling \code{BUGS} models.

}
\author{
Authors: \enc{P\'eter S\'olymos}{Peter Solymos}

Maintainer: \enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}
}
\references{
Solymos, P., 2010. dclone: Data Cloning in R.
\emph{The R Journal} \strong{2(2)}, 29--37.
URL: \url{http://journal.r-project.org/}

Lele, S.R., B. Dennis and F. Lutscher, 2007.
Data cloning: easy maximum likelihood estimation for complex 
ecological models using Bayesian Markov chain Monte Carlo  methods.
\emph{Ecology Letters} \strong{10}, 551--563.

Lele, S. R., K. Nadeem and B. Schmuland, 2010.
Estimability and likelihood inference for generalized linear mixed models using data cloning.
\emph{Journal of the American Statistical Association}
\strong{xx}, in press.
}
\keyword{ package }
