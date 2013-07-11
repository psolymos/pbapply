.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
        fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    invisible(NULL)
}

.onLoad <- function(libname, pkgname){
    ## dcoptions setup
    if (is.null(getOption("dcoptions")))
        options("dcoptions"=list(
            "autoburnin"=TRUE,
            "diag"=0.05,
            "LB"=FALSE,
            "overwrite"=TRUE,
            "RNG"="none",
            "rhat"=1.1,
            "verbose"=1))
    invisible(NULL)
}

.onUnload <- function(libpath){
    ## remove dcoptions
    options("dcoptions"=NULL)
    invisible(NULL)
}

