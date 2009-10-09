#.First.lib <- function(lib, pkg){
#    cat("This is dclone ", utils::packageDescription("dclone", field="Version"),
#    " (", utils::packageDescription("dclone", field="Date"), ")\n", sep="")
#    if (is.null(getOption("dclone.crit"))) {
#        options("dclone.crit"=c(lmax=0.05, pshw=0.05, rhat=1.1))
#    }
#}

.onAttach <- function(libname,pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                    fields=c("Version", "Date"))
    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    invisible(NULL)
}

.onUnload <- function(){
    invisible(NULL)
}

## todo
##
## + 1 param case: shapiro.test, unscaled SD
##   (plus rename mshapiro and mstp) -- done
## + dctable: $convergence & $statistics
##   within $statistics list by parameters
##   (k * stats matrix) -- done## * remove default =1 in dclone ???
## + rename jags.fit.dclone to dcjags or jags.dclone
##   (similarly dcbugs/bugs.dclone in future)
## * rename report???
## * develop spts class for space-time series
##   and dclone.spts, dclone.ts (??? mspts, mts)
## * inits function -- just in example
## * data$priors and priors function
## * shapiro.diag: 5000 to decrease until memory is o.k.
##   with try()
## * store MCMC results in jags.dclone???

#vcov.mcmc.list <- function(object, ...) cov(report(object, array))

#quantile.mcmc.list <- function(x, ...) {
#    apply(report(x, array), 2, quantile, ...)
#}

#as.mcmc.list.dc <- function(object) {
#    class(object) <- c("mcmc.list.dc", class(object))
#    attr(object, "n.clones") <- nclones(object)
#    object
#}

#make.symmetric <- function(x) {
#    for (i in 1:nrow(x)) {
#        for (j in 1:ncol(x)) {
#            tmp <- mean(x[i,j], x[j,i])
#            x[i,j] <- x[j,i] <- tmp
#   }}
#    x
#}
