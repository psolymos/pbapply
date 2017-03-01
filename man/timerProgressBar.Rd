\name{timerProgressBar}
\alias{timerProgressBar}
\alias{setTimerProgressBar}
\alias{getTimerProgressBar}
\title{
Timer Progress Bar
}
\description{
Text progress bar with timer in the R console.
}
\usage{
timerProgressBar(min = 0, max = 1, initial = 0, char = "=",
    width = NA, title, label, style = 1, file = "", min_time = 0)
getTimerProgressBar(pb)
setTimerProgressBar(pb, value, title = NULL, label = NULL)
}
\arguments{
  \item{min, max}{
(finite) numeric values for the extremes of the progress bar.
Must have \code{min} < \code{max}.
}
  \item{initial, value}{
initial or new value for the progress bar.
See Details for what happens with invalid values.
}
  \item{char}{
he character (or character string) to form the progress bar.
If number of characters is >1, it is silently stripped to length 1
unless \code{style} is 5 or 6 (see Details).
}
  \item{width}{
the width of the progress bar, as a multiple of the width of char.
If \code{NA}, the default, the number of characters is that
which fits into \code{getOption("width")}.
}
  \item{style}{
the style taking values between 1 and 6.
1: progress bar with elapsed and remaining time,
remaining percentage is indicated by spaces between pipes
(default for this function),
2: throbber with elapsed and remaining time,
3: progress bar with remaining time printing elapsed time at the end,
remaining percentage is indicated by spaces between pipes
(default for \code{style} option in \code{\link{pboptions}}),
4: throbber with remaining time printing elapsed time at the end,
5: progress bar with elapsed and remaining time
with more flexible styling (see Details and Examples),
6: progress bar with remaining time printing elapsed time at the end
with more flexible styling (see Details and Examples).
}
  \item{file}{
an open connection object or \code{""} which indicates the console.
}
  \item{min_time}{
numeric, minimum processing time (in seconds) required to show a progress bar.
}
  \item{pb}{
an object of class \code{"timerProgressBar"}.
}
  \item{title, label}{
ignored, for compatibility with other progress bars.
}
}
\details{
\code{timerProgressBar} will display a progress bar on the R console
(or a connection) via a text representation.

\code{setTimerProgessBar} will update the value. Missing (\code{NA}) and out-of-range values of value will be (silently) ignored. (Such values of \code{initial}
cause the progress bar not to be displayed until a valid value is set.)

The progress bar should be closed when finished with: this outputs the final newline character (see \code{\link{closepb}}).

If \code{style} is 5 or 6, it is possible to define up to 4 characters
for the \code{char} argument (as a single string) for the left end,
elapsed portion, remaining portion, and right end of the progress bar
(\code{|= |} by default). Remaining portion cannot be the same as the
elapsed portion (space is used for remaining in such cases).
If 1 character is defined, it is taken for the elapsed portion.
If 2-4 characters are defined, those are interpreted in sequence
(left and right end being the same when 2-3 characters defined),
see Examples.
}
\value{
For \code{timerProgressBar} an object of class \code{"timerProgressBar"}
inheriting from \code{"txtProgressBar"}.

For \code{getTimerProgressBar} and \code{setTimerProgressBar},
a length-one numeric vector giving the previous
value (invisibly for \code{setTimerProgressBar}).
}
\author{
Zygmunt Zawadzki <zawadzkizygmunt@gmail.com>

Peter Solymos <solymos@ualberta.ca>
}
\seealso{
The \code{timerProgressBar} implementation
follows closely the code of \code{\link[utils]{txtProgressBar}}.
}
\examples{
## increase sluggishness to admire the progress bar longer
sluggishness <- 0.02

test_fun <- function(...)
{
    pb <- timerProgressBar(...)
    on.exit(close(pb))
    for(i in seq(0, 1, 0.05)) {
        Sys.sleep(sluggishness)
        setTimerProgressBar(pb, i)
    }
    invisible(NULL)
}

## check the different styles
test_fun(width = 35, char = "+", style = 1)
test_fun(style = 2)
test_fun(width = 50, char = ".", style = 3)
test_fun(style = 4)
test_fun(width = 35, char = "[=-]", style = 5)
test_fun(width = 50, char = "{*.}", style = 6)

## no bar only percent and elapsed
test_fun(width = 0, char = "    ", style = 6)

## this should produce a progress bar based on min_time
(elapsed <- system.time(test_fun(width = 35, min_time = 0))["elapsed"])
## this should not produce a progress bar based on min_time
system.time(test_fun(min_time = 2 * elapsed))["elapsed"]
}
\keyword{ utilities }
