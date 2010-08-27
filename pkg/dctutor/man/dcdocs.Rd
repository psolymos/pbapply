\encoding{UTF-8}
\name{dcdocs}
\alias{dcdocs}
\title{Display Package Documentation}
\description{
  Display package documentation using \code{pager} or \code{pdfviewer}
  defined in \code{\link{options}}.
}
\usage{
mefadocs(doc = c("dclone","dcglm","dcpar"))
}
\arguments{
  \item{doc}{The name of the document (partial match, case sensitive). }
}
\author{Peter Solymos \email{solymos@ualberta.ca, adapted Jari Oksanen's \code{\link[vegan]{vegandocs}}}
\examples{
\dontrun{
dcdocs("dclone")
}
}
\keyword{ documentation }
