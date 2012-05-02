\name{dcmle-classes}
\alias{dcmle-classes}
\alias{makeGsFit}
\alias{makeDcFit}
\alias{dcMle}
\alias{gsFit}
\alias{dcFit}
\alias{dcMle-class}
\alias{gsFit-class}
\alias{dcFit-class}
\alias{show,dcMle-method}
\alias{show,dcFit-method}
\alias{show,gsFit-method}
\alias{custommodel}
\alias{custommodel-class}
\alias{mcmc}
\alias{mcmc-class}
\alias{mcmc.list}
\alias{mcmc.list-class}
\alias{mcmc.list}
\alias{mcmc.list.dc-class}
\alias{dctable}
\alias{dctable-class}
\alias{dcdiag}
\alias{dcdiag-class}
\alias{nClones}
\alias{nClones-class}
\alias{dcDiag}
\alias{dcDiag-class}
\alias{dcTable}
\alias{dcTable-class}
\alias{MCMClist}
\alias{MCMClist-class}
\alias{dcArgs}
\alias{dcArgs-class}
\alias{dcParams}
\alias{dcParams-class}
\alias{dcFunction}
\alias{dcFunction-class}
\alias{dcInits}
\alias{dcInits-class}
\alias{dcModel}
\alias{dcModel-class}
\title{
'dcMle' Classes
}
\description{
Basic S4 object classes defined in the \pkg{dcmle} package,
and corresponding creator functions.
}
\usage{
makeGsFit(data, model, params = NULL, inits = NULL)

makeDcFit(data, model, params=NULL, inits = NULL,
    multiply = NULL, unchanged = NULL, update = NULL,
    updatefun = NULL, initsfun = NULL, flavour)
}
\arguments{
  \item{data}{usually a named list with data.}
  \item{model}{BUGS model (function, character vector or
    a \code{\link[dclone]{custommodel}} object).
    The argument is coerced into a \code{custommodel} object.}
  \item{params}{optional, character vector for model
    parameters to monitor.}
  \item{inits}{initial values (\code{NULL}, list or function).}
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
\details{
'gsFit' (after BU*GS*/JA*GS*) is a basic object class representing 
requirements for the Bayesian MCMC model fitting.
The 'dcFit' object class extends 'gsFit'
by additional slots that are used to fine tune
how data cloning is done during fitting process.
Both 'gsFit' and 'dcFit' represent prerequisites
for model fitting, but do not containing any fitted
parts. Creator functions \code{makeGsFit} and \code{makeDcFit}
are available for these classes.
See \code{\link{dcmle-package}} help page for usage of creator functions.

The 'dcMle' object is a fitted model object
that is created based on the model representation
in 'gsFit' and 'dcFit'. Fitting is done by
the \code{\link{dcmle}} function and described on
separate help page.

The default \code{flavour} is stored in 
\code{getOption("dcmle.flavour")} with value \code{"jags"}.
It can be changed as \code{options("dcmle.flavour"="bugs")}
if required.
}
\value{
The class 'gsFit' has the following slots
(\code{data} and \code{model} are essential, others are optional):
\itemize{
  \item{\code{data}:}{ usually a named list with data.}
  \item{\code{model}:}{ BUGS model (function, character vector or
    a \code{\link[dclone]{custommodel}} object).}
  \item{\code{params}:}{ optional, character vector for model
    parameters to monitor.}
  \item{\code{inits}:}{ initial values (\code{NULL}, list or function).}
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
  \item{\code{mcmc}:}{ a fitted 'mcmc.list' object.}
  \item{\code{summary}:}{ Wald-type summary based on the 'mcmc.list'.}
  \item{\code{dctable}:}{ summary statistics of iterative fitting 
    (see \code{\link[dclone]{dctable}}).}
  \item{\code{dcdiag}:}{ data cloning diagnostics 
    (see \code{\link[dclone]{dcdiag}}).}
  \item{\code{start}:}{ first MCMC iteration.}
  \item{\code{end}:}{ last MCMC iteration.}
  \item{\code{thin}:}{ thinning value used for MCMC chain.}
  \item{\code{n.chains}:}{ number of MCMC chains.}
  \item{\code{n.clones}:}{ number of clones of the final mcmc.list.}
}

The classes 'custommodel', 'mcmc', 'mcmc.list',
'mcmc.list.dc', 'dctable', 'dcdiag' are virtual S4 classes
defined after S3 representatives in the \pkg{coda}
and \pkg{dclone} packages.

The classes 'nClones', 'dcDiag', 'dcTable', 'MCMClist', 'dcArgs',
'dcParams', 'dcFunction', 'dcInits', 'dcModel' are
class unions for slots of 'gsFit', 'dcFit' and 'dcMle'.
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
