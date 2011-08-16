## >> checked and worked with svocc class, May 11, 2010 PS <<

.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    if (is.null(getOption("detect.optim.control")))
        options("detect.optim.control"=list(maxit = 20000))
#    if (is.null(getOption("detect.optim.method")))
#        options("detect.optim.method"="Nelder-Mead")
    if (is.null(getOption("detect.mcmc.control")))
        options("detect.mcmc.control"=list(n.chains = 3, n.adapt = 1000, n.update = 0, thin = 1, n.iter = 5000))
    if (is.null(getOption("detect.dc.control")))
        options("detect.dc.control"=list(n.clones = 1000, prec = 0.1))
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("detect.optim.control"=NULL)
#    options("detect.optim.method"=NULL)
    options("detect.mcmc.control"=NULL)
    options("detect.dc.control"=NULL)
    invisible(NULL)
}

