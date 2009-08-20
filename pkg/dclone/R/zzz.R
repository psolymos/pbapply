.First.lib <- function(lib, pkg){
    cat("This is dclone ", utils::packageDescription("dclone", field="Version"),
    " (", utils::packageDescription("dclone", field="Date"), ")\n", sep="")
    if (is.null(getOption("dclone.crit"))) {
        options("dclone.crit"=c(lmax=0.05, pshw=0.05, rhat=1.1))
    }
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