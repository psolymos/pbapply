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
Basic S4 object classes defined in the \pkg{dcmle} package.
}
\details{
'gsFit' (after BU*GS*/JA*GS*) is a basic object class representing 
requirements for the Bayesian MCMC model fitting.
The 'dcFit' object class extends 'gsFit'
by additional slots that are used to fine tune
how data cloning is done during fitting process.
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
  \item{\code{data}:}{ a named list with data.}
  \item{\code{inits}:}{ initial values (\code{NULL}, list or function).}
  \item{\code{model}:}{ BUGS model (function, character vector or
    a \code{\link[dclone]{custommodel}} object).}
  \item{\code{params}:}{ optional, character vector for model
    parameters to monitor.}
}

The class 'dcFit' has the following optional slots besides the 
ones from 'gsFit':
\itemize{
  \item{\code{multiply}:}{ optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{\code{unchanged}:}{ optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{\code{update}:}{ optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{\code{updatefun}:}{ optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{\code{initsfun}:}{ optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
  \item{\code{flavour}:}{ optional, argument passed to 
    \code{\link[dclone]{dc.fit}}.}
}

The class 'dcMle' has the following slots:
\itemize{
  \item{\code{mcmc}:}{ a fitted mcmc.list object.}
  \item{\code{summary}:}{ Wald-type summary based on the mcmc.list.}
  \item{\code{dctable}:}{ summary statistics of iterative fitting (\code{\link[dclone]{dctable}}).}
  \item{\code{dcdiag}:}{ data cloning diagnostics (\code{\link[dclone]{dcdiag}}).}
  \item{\code{start}:}{ first MCMC iteration.}
  \item{\code{end}:}{ last MCMC iteration.}
  \item{\code{thin}:}{ thinning value used for MCMC chain.}
  \item{\code{n.chains}:}{ number of MCMC chains.}
  \item{\code{n.clones}:}{ number of clones of the final mcmc.list.}
}
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
Fitting wrapper function: \code{\link{dcmle}}

Methods: \code{\link{dcmle-methods}}
}
\examples{
new("gsFit")
new("dcFit")
data(regmod)
as(regmod, "dcMle")
}
\keyword{ classes }
