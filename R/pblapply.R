pblapply <-
function (X, FUN, ..., cl = NULL)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    ## catch single node requests and forking on Windows
    if (!is.null(cl)) {
        if (inherits(cl, "cluster") && length(cl) < 2L)
            cl <- NULL
        if (!inherits(cl, "cluster") && cl < 2)
            cl <- NULL
        if (!inherits(cl, "cluster") && .Platform$OS.type == "windows")
            cl <- NULL
    }
    nout <- as.integer(getOption("pboptions")$nout)
    ## sequential evaluation
    if (is.null(cl)) {
        if (!(interactive() && dopb() && length(X) >= 1))
            return(lapply(X, FUN, ...))
        #pb <- startpb(0, B)
        #on.exit(closepb(pb), add = TRUE)
        #rval <- vector("list", B)
        #for (i in seq_len(B)) {
        #    rval[i] <- list(FUN(X[[i]], ...))
        #    setpb(pb, i)
        #}
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
            if (!(interactive() && dopb() && length(X) >= 1))
                return(parallel::parLapply(cl, X, FUN, ...))
            ## define split here and use that for counter
            Split <- splitpb(length(X), length(cl), nout = nout)
            B <- length(Split)
            pb <- startpb(0, B)
            on.exit(closepb(pb), add = TRUE)
            rval <- vector("list", B)
            for (i in seq_len(B)) {
                rval[i] <- list(parallel::parLapply(cl, X[Split[[i]]], FUN, ...))
                setpb(pb, i)
            }
        ## multicore type forking
        } else {
            if (!(interactive() && dopb() && length(X) >= 1))
                return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
            ## define split here and use that for counter
            Split <- splitpb(length(X), as.integer(cl), nout = nout)
            B <- length(Split)
            pb <- startpb(0, B)
            on.exit(closepb(pb), add = TRUE)
            rval <- vector("list", B)
            for (i in seq_len(B)) {
                rval[i] <- list(parallel::mclapply(X[Split[[i]]], FUN, ...,
                    mc.cores = as.integer(cl)))
                setpb(pb, i)
            }
        }
    }
    ## assemble output list
    rval <- do.call(c, rval, quote = TRUE)
    names(rval) <- names(X)
    rval
}
