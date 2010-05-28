\name{sardata01}
\Rdversion{1.1}
\alias{sardata01}
\docType{data}
\title{
Data Sets for the Study of the Species-Area Relationship, No. 01
}
\description{
Data sets for the study of the species-srea relationship
}
\usage{data(sardata01)}
\format{
  A list of two data frames, see Details.
}
\details{
The element \code{sardata$sar} is a data frame with variables:
  \describe{
    \item{\code{study}}{Factor, levels are the study identifiers.}
    \item{\code{id}}{Numeric, island identifiers according to the original references.}
    \item{\code{S}}{Numeric, number of species according to the original references.}
    \item{\code{A}}{Numeric, area of the island in square kilometers, according to the original references.}
  }

The element \code{sardata$study} is a data frame with variables:
  \describe{
    \item{\code{study}}{Factor, levels are the study identifiers.}
    \item{\code{taxon.group}}{Factor, taxonomic group.}
    \item{\code{island.type}}{Factor, island type.}
    \item{\code{latitude}}{Numeric, latitude for the sudy close to the equator.}
    \item{\code{extent}}{Numeric, latitudinal extent of the study.}
    \item{\code{location}}{Character, location of the study.}
  }
}
\references{
References are in the files \code{sardata01.txt} and \code{sardata01.bib} in the \code{sardata} library of \code{R}, labelled by study identifiers. See examples on how to recall the files from the console.
}
\examples{
## data structure
data(sardata01)
str(sardata01$sar)
str(sardata01$study)
## references
file.show(system.file(package = "sardata", "sardata01.txt"))
file.show(system.file(package = "sardata", "sardata01.bib"))
}
\keyword{datasets}
