\name{pboptions}
\alias{pboptions}
\title{
Sets Options for PRogress Bar
}
\description{
Sets Options for PRogress Bar
}
\usage{
pboptions(pb, txt, gui)
}
\arguments{
  \item{pb}{
Progress bar option.
}
  \item{txt}{
Text progress bar options.
}
  \item{gui}{
GUI progress bar options.
}
}
\details{
FIXME
}
\value{
FIXME
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
FIXME
}
\examples{
options("pbapply.txt"=getOption("pbapply.text"))
pb <- startpb(0, 10)
for(i in 1:10) {
    Sys.sleep(0.2)
    setpb(pb, i)
}
closepb(pb)
## resetting pboptions
opb <- pboptions(txt=list(style=1, char="*"))
## check new settings
getOption("pbapply.txt")
## running again with new settings
pb <- startpb(0, 10)
for(i in 1:10) {
    Sys.sleep(0.2)
    setpb(pb, i)
}
closepb(pb)
## resetting original
pboptions(opb)
## check reset
getOption("pbapply.txt")
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
