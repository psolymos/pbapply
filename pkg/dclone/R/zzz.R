.onAttach <- function(libname, pkgname){
    ## dcoptions setup
    if (is.null(getOption("dcoptions")))
        options("dcoptions"=list("rhat"=1.1,
            "autoburnin"=TRUE,
            "diag"=0.05,
            "verbose"=1,
            "LB"=FALSE,
            "RNG"="none",
            "single.par.model"=TRUE))
    rj <- suppressWarnings(require(rjags))
    if (!rj)
        cat("Warning message:\n  there is no package called 'rjags'\n")
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
        fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    invisible(NULL)
}

.onUnload <- function(libpath){
    ## remove dcoptions
    options("dcoptions"=NULL)
    invisible(NULL)
}
