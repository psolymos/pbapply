.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
        fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    invisible(NULL)
}

.onLoad <- function(libname, pkgname){
    if (is.null(getOption("dcmle.flavour")))
        options("dcmle.flavour"="jags")
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("dcmle.flavour"=NULL)
    invisible(NULL)
}

