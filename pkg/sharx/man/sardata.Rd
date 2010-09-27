\name{sardata}
\Rdversion{1.1}
\alias{sardata}
\docType{data}
\title{
Data Sets for the Study of the Species-Area Relationship
}
\description{
Data sets for the study of the species-srea relationship
}
\usage{data(sardata)}
\format{
  A list of two data frames, see Details.
}
\details{
The element \code{sardata$islands} is a data frame with variables:
  \describe{
    \item{\code{study}}{Factor, levels are the study identifiers.}
    \item{\code{id}}{Numeric, island identifiers according to the original references.}
    \item{\code{S}}{Numeric, number of species according to the original references.}
    \item{\code{A}}{Numeric, area of the island in square kilometers, according to the original references.}
  }

The element \code{sardata$studies} is a data frame with variables:
  \describe{
    \item{\code{study}}{Factor, levels are the study identifiers.}
    \item{\code{taxon.group}}{Factor, taxonomic group.}
    \item{\code{island.type}}{Factor, island type.}
    \item{\code{latitude}}{Numeric, midlle band of latitude for the sudy calculated as (min + max) / 2, 
      where min is the latitude close to the Equator, max is the latitude close to the poles.}
    \item{\code{extent}}{Numeric, latitudinal extent of the study.}
    \item{\code{location}}{Character, location of the study.}
  }
}
\references{
References are in the files \code{sardata.txt} and \code{sardata.bib} in the \code{sharx} library of \code{R}, labelled by study identifiers. See examples on how to recall the files from the console.
}
\examples{
## data structure
data(sardata)
str(sardata$islands)
str(sardata$studies)
## references
file.show(system.file(package = "sharx", "sardata.txt"))
file.show(system.file(package = "sharx", "sardata.bib"))
}
\keyword{datasets}
