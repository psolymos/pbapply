pblapply <-
function (X, FUN, ..., cl = NULL)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    if (!length(X))
        return(lapply(X, FUN, ...))
    ## catch single node requests and forking on Windows
    if (!is.null(cl)) {
        if (.Platform$OS.type == "windows" | !identical(cl, "future")) {
            if (!inherits(cl, "cluster"))
                cl <- NULL
        } else {
            if (inherits(cl, "cluster")) {
                if (length(cl) < 2L)
                    cl <- NULL
            } else {
                if (cl < 2)
                    cl <- NULL
            }
        }
        ## deal with future
        if (identical(cl, "future") && (!requireNamespace("future") || !requireNamespace("future.apply"))) {
            warning("You need some packages for cl='future' to work: install.packages('future.apply')")
            cl <- NULL
        }
    }
    nout <- as.integer(getOption("pboptions")$nout)
    ## sequential evaluation
    if (is.null(cl)) {
        if (!dopb())
            return(lapply(X, FUN, ...))
        Split <- splitpb(length(X), 1L, nout = nout)
        B <- length(Split)
        pb <- startpb(0, B)
        on.exit(closepb(pb), add = TRUE)
        rval <- vector("list", B)
        for (i in seq_len(B)) {
            rval[i] <- list(lapply(X[Split[[i]]], FUN, ...))
            setpb(pb, i)
        }
    ## parallel evaluation
    } else {
        ## snow type cluster
        if (inherits(cl, "cluster")) {
            ## switch on load balancing if needed
            PAR_FUN <- if (isTRUE(getOption("pboptions")$use_lb))
                parallel::parLapplyLB else parallel::parLapply
            if (!dopb())
                return(PAR_FUN(cl, X, FUN, ...))
            ## define split here and use that for counter
            Split <- splitpb(length(X), length(cl), nout = nout)
            B <- length(Split)
            pb <- startpb(0, B)
            on.exit(closepb(pb), add = TRUE)
            rval <- vector("list", B)
            for (i in seq_len(B)) {
                rval[i] <- list(PAR_FUN(cl, X[Split[[i]]], FUN, ...))
                setpb(pb, i)
            }
        ## future backend
        } else if (identical(cl, "future")) {
            requireNamespace("future")
            requireNamespace("future.apply")
            if (!dopb())
                return(future.apply::future_lapply(X, FUN, ...,
                    future.stdout = FALSE))
            Split <- splitpb(length(X), future::nbrOfWorkers(), nout = nout)
            B <- length(Split)
            pb <- startpb(0, B)
            on.exit(closepb(pb), add = TRUE)
            rval <- vector("list", B)
            for (i in seq_len(B)) {
                rval[i] <- list(future.apply::future_lapply(X[Split[[i]]], FUN, ...,
                    future.stdout = FALSE))
                setpb(pb, i)
            }
        ## multicore type forking
        } else {
            if (!dopb())
                return(parallel::mclapply(X, FUN, ..., 
                    mc.cores = as.integer(cl),
                    mc.silent = TRUE))
            ## define split here and use that for counter
            Split <- splitpb(length(X), as.integer(cl), nout = nout)
            B <- length(Split)
            pb <- startpb(0, B)
            on.exit(closepb(pb), add = TRUE)
            rval <- vector("list", B)
            for (i in seq_len(B)) {
                rval[i] <- list(parallel::mclapply(X[Split[[i]]], FUN, ...,
                    mc.cores = as.integer(cl),
                    mc.silent = TRUE))
                setpb(pb, i)
            }
        }
    }
    ## assemble output list
    rval <- do.call(c, rval, quote = TRUE)
    names(rval) <- names(X)
    rval
}
