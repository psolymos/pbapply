\name{pboptions}
\alias{pboptions}
\alias{startpb}
\alias{setpb}
\alias{getpb}
\alias{closepb}
\alias{dopb}
\alias{doshiny}
\alias{pbtypes}
\title{
Creating Progress Bar and Setting Options
}
\description{
Creating progress bar and setting options.
}
\usage{
pboptions(...)
startpb(min = 0, max = 1)
setpb(pb, value)
getpb(pb)
closepb(pb)
dopb()
doshiny()
pbtypes()
}
\arguments{
  \item{\dots}{
Arguments in \code{tag = value} form, or a list of tagged values.
The tags must come from the parameters described below.
}
  \item{pb}{
A progress bar object created by \code{startpb}.
}
  \item{min, max}{
Finite numeric values for the extremes of the progress bar.
Must have \code{min < max}.
}
  \item{value}{
New value for the progress bar.
}
}
\details{
\code{pboptions} is a convenient way of handling options
related to progress bar.

Other functions can be used for conveniently adding progress
bar to \code{for}-like loops
(see Examples).
}
\value{
When parameters are set by \code{pboptions}, their former values are
returned in an invisible named list. Such a list can be passed as an
argument to \code{pboptions} to restore the parameter values.
Tags are the following:

  \item{type}{Type of the progress bar: timer (\code{"timer"}),
    text (\code{"txt"}),
    Windows (\code{"win"}), TclTk (\code{"tk"}), or none (\code{"none"}).
    Default value is \code{"timer"} progress bar with estimated remaining time
    when in interactive mode, and \code{"none"} otherwise.
    See \code{pbtypes()} for available progress bar types
    depending on operating system.}
  \item{char}{The character (or character string) to form the progress bar.
    Default value is \code{"+"}.}
  \item{txt.width}{The width of the text based progress bar, as a multiple
    of the width of \code{char}.
    If \code{NA}, the number of characters is that which fits into
    \code{getOption("width")}.
    Default value is \code{50}.}
  \item{gui.width}{The width of the GUI based progress bar in pixels:
    the dialogue box will be 40 pixels wider (plus frame).
    Default value is \code{300}.}
  \item{style}{The style of the bar, see
    \code{\link[utils]{txtProgressBar}} and \code{\link{timerProgressBar}}.
    Default value is \code{3}.}
  \item{initial}{Initial value for the progress bar. Default value is
    \code{0}.}
  \item{title}{Character string giving the window title
    on the GUI dialogue box. Default value is \code{"R progress bar"}.}
  \item{label}{Character string giving the window label
    on the GUI dialogue box. Default value is \code{""}.}
  \item{nout}{Integer, the maximum number of times the progress bar is updated.
    The default value is 100. Smaller value minimizes the
    running time overhead related to updating the progress bar.
    This can be especially important for forking type parallel runs.}
  \item{min_time}{Minimum time in seconds.
    \code{\link{timerProgressBar}} output is printed only if
    estimated completion time is higher than this value.
    The default value is 0.}
  \item{use_lb}{Switch for using load balancing when running in
    parallel clusters. The default value is \code{FALSE}.}

For \code{startpb} a progress bar object.

For \code{getpb} and \code{setpb}, a length-one numeric vector giving
the previous value (invisibly for \code{setpb}).
The return value is \code{NULL} if the progress bar is turned off by
\code{getOption("pboptions")$type} (\code{"none"} or \code{NULL} value).

\code{dopb} returns a logical value if progress bar is to be shown
based on the option \code{getOption("pboptions")$type}.
It is \code{FALSE} if the type of progress bar is \code{"none"} or
\code{NULL}.

\code{doshiny} returns a logical value, \code{TRUE} when the shiny
package namespace is available (i.e. the suggested package is installed),
the \code{type} option is set to \code{"shiny"}, and a shiny application
is running.

For \code{closepb} closes the connection for the progress bar.

\code{pbtypes} prints the available progress bar types
depending on the operating system (i.e. \code{"win"} available
on Windows only).
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
Progress bars used in the functions:
#ifdef windows
\code{\link[utils]{winProgressBar}},
#endif
\code{\link{timerProgressBar}},
\code{\link[utils]{txtProgressBar}},
\code{\link[tcltk]{tkProgressBar}}
}
\examples{
## increase sluggishness to admire the progress bar longer
sluggishness <- 0.01

## for loop
fun1 <- function() {
    pb <- startpb(0, 10)
    on.exit(closepb(pb))
    for (i in 1:10) {
        Sys.sleep(sluggishness)
        setpb(pb, i)
    }
    invisible(NULL)
}
## while loop
fun2 <- function() {
    pb <- startpb(0, 10-1)
    on.exit(closepb(pb))
    i <- 1
    while (i < 10) {
        Sys.sleep(sluggishness)
        setpb(pb, i)
        i <- i + 1
    }
    invisible(NULL)
}
## using original settings
fun1()
## resetting pboptions
opb <- pboptions(style = 1, char = ">")
## check new settings
getOption("pboptions")
## running again with new settings
fun2()
## resetting original
pboptions(opb)
## check reset
getOption("pboptions")
fun1()

## dealing with nested progress bars
## when only one the 1st one is needed
f <- function(x) Sys.sleep(sluggishness)
g <- function(x) pblapply(1:10, f)
tmp <- lapply(1:10, g) # undesirable
## here is the desirable solution
h <- function(x) {
    opb <- pboptions(type="none")
    on.exit(pboptions(opb))
    pblapply(1:10, f)
}
tmp <- pblapply(1:10, h)

## list available pb types
pbtypes()
}
\keyword{ IO }
\keyword{ utilities }
