.onAttach <- function(libname, pkgname){
#    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
#                    fields=c("Version", "Date"))
#    cat(paste(pkgname, ver[1], "\t", ver[2], "\n"))
    if (is.null(getOption("pbapply.pb")))
        options("pbapply.pb"="txt")
    if (is.null(getOption("pbapply.gui")))
        options("pbapply.gui"=list(title="R progress bar",
            label="", width=300, initial=0))
    if (is.null(getOption("pbapply.txt")))
        options("pbapply.txt"=list(char="+", width=50, style=3, initial=0))

    invisible(NULL)
}

.onUnload <- function(libpath){
    ## remove dclone options
    options("pbapply.pb"=NULL)
    options("pbapply.gui"=NULL)
    options("pbapply.txt"=NULL)
    invisible(NULL)
}

