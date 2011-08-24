.onAttach <- function(libname, pkgname){
    ## dcoptions setup
    if (is.null(getOption("dcoptions")))
        options("dcoptions"=list(
            "autoburnin"=TRUE,
            "diag"=0.05,
            "LB"=FALSE,
            "overwrite"=FALSE,
            "RNG"="none",
            "rhat"=1.1,
            "verbose"=1))
    rj <- suppressWarnings(try(require(rjags), silent=TRUE))
    ## no need for warning if error is produced (e.g. JAGS .dll not found)
    rj <- if (inherits(rj, "try-error"))
        TRUE
    if (!rj)
        cat("Warning message:\n  there is no package called 'rjags'\n")
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
        fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    invisible(NULL)
}

.onUnload <- function(libpath){
    ## remove dcoptions
    options("dcoptions"=NULL)
    invisible(NULL)
}

