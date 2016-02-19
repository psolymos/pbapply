\name{pboptions}
\alias{pboptions}
\alias{startpb}
\alias{setpb}
\alias{getpb}
\alias{closepb}
\alias{dopb}
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
    Default value is \code{"timer"} progress bar with estimated remaining time.}
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
    \code{\link[utils]{txtProgressBar}}. Default value is \code{3}.}
  \item{initial}{Initial value for the progress bar. Default value is
    \code{0}.}
  \item{title}{Character string giving the window title
    on the GUI dialogue box. Default value is \code{"R progress bar"}.}
  \item{label}{Character string giving the window label
    on the GUI dialogue box. Default value is \code{""}.}

For \code{startpb} a progress bar object.

For \code{getpb} and \code{setpb}, a length-one numeric vector giving
the previous value (invisibly for \code{setpb}).
The return value is \code{NULL} if the progress bar is turned off by
\code{getOption("pbapply.pb")} (\code{"none"} or \code{NULL} value).

\code{dopb} returns a logical value if progress bar is to be shown
based on the option \code{getOption("pbapply.pb")}.
It is \code{FALSE} if the type of progress bar is \code{"none"} or
\code{NULL}.

For \code{closepb} closes the connection for the progress bar.
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
## for loop
fun1 <- function() {
    pb <- startpb(0, 10)
    on.exit(closepb(pb))
    for (i in 1:10) {
        Sys.sleep(0.15)
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
        Sys.sleep(0.15)
        setpb(pb, i)
        i <- i + 1
    }
    invisible(NULL)
}
## using original settings
fun1()
## resetting pboptions
opb <- pboptions(style=1, char=">")
## check new settings
getOption("pboptions")
## running again with new settings
fun2()
## resetting original
pboptions(opb)
## check reset
getOption("pboptions")
fun1()
}
\keyword{ IO }
\keyword{ utilities }
