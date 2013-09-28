evalParallelArgument <- 
function(cl, quit=FALSE) {
    if (missing(cl))
        cl <- NULL
    if (is.null(cl)) {
        mc <- getOption("mc.cores")
#        dcl <- if (getRversion() >= "2.15.0")
#            get("default", envir = parallel:::.reg) else NULL
        dcl <- NULL # CRAN policies prohibit this usage
        ## stop if default is ambiguous
        if (!is.null(mc) && !is.null(dcl))
            if (quit)
                stop("cannot decide default parallel type (cl = NULL)")
            cl <- NULL
        ## us mc.cores if it is not 1
        if (!is.null(mc) && is.null(dcl))
            cl <- if (mc < 2)
                NULL else mc
        ## or use default cluster
        if (is.null(mc) && !is.null(dcl))
            cl <- dcl
        ## stop if cl is still NULL
        if (quit && is.null(cl))
            stop("ambiguous default parallel type definition")
    } else {
        ## sequential if cl=1
        if (is.numeric(cl)) {
            if (cl < 2) {
                if (quit)
                    stop("use sequential functions or set mc.cores to >1")
                cl <- NULL
            }
            cl <- as.integer(cl)
        }
    }
    cl
}
