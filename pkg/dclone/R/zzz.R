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
    if (is.null(getOption("dclone.crit"))) {
        options("dclone.crit"=c(lmax=0.05, pshw=0.05, rhat=1.1))
    }
    invisible(NULL)
}

.onUnload <- function(){
    if (!is.null(getOption("dclone.crit"))) {
        options("dclone.crit"=NULL)
    }
    invisible(NULL)
}

