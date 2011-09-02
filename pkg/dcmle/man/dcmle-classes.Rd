\name{dcmle-classes}
\alias{dcmle-classes}
\alias{dcMle}
\alias{gsFit}
\alias{dcFit}
\alias{dcMle-class}
\alias{gsFit-class}
\alias{dcFit-class}
\title{
'dcMle' Classes
}
\description{
Basic S4 object classes defined in the package
}
\details{
'gsFit' is a basic object class representing 
requirements for the Bayesian MCMC model fitting.
The 'dcFit' object class extends 'gsFit'
by additional slots that are used to fine tune
how data cloning is done during fitting the model.
Both 'gsFit' and 'dcFit' represent prerequisites
for model fitting, but do not containing any fitted
parts.

The 'dcMle' object is a fitted model object
that is created based on the model representation
in 'gsFit' and 'dcFit'. Fitting is done by
the \code{\link{dcmle}} function.
}
\value{
The class 'gsFit' has the following slots
(data and model are essential, others are optional):
\itemize{
  \item{data}{a named list with data.}
  \item{inits}{initial values (\code{NULL}, list or function).}
  \item{model}{BUGS model (function, character vector or
    a \code{\link[dclone]{custommodel}} object).}
  \item{params}{optional, character vector for model
    parameters to monitor.}
}

The class 'dcFit' has the following optional slots besides the 
ones from 'gsFit':
\itemize{
  \item{multiply}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{unchanged}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{update}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{updatefun}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{initsfun}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{flavour}{optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
}

The class 'dcMle' has the following slots:
\itemize{
  \item{mcmc}{a fitted mcmc.list object.}
  \item{summary}{Wald-type summary based on the mcmc.list.}
  \item{dctable}{summary statistics of iterative fitting (\code{\link[dclone]{dctable}}).}
  \item{dcdiag}{data cloning diagnostics (\code{\link[dclone]{dcdiag}}).}
  \item{start}{first MCMC iteration.}
  \item{end}{last MCMC iteration.}
  \item{thin}{thinning value used for MCMC chain.}
  \item{n.chains}{number of MCMC chains.}
  \item{n.clones}{number of clones of the final mcmc.list.}
}
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{dcmle}}
}
\keyword{ classes }
