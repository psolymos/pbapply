pbapply <-
function (X, MARGIN, FUN, ..., cl = NULL)
{
    FUN <- match.fun(FUN)
    dl <- length(dim(X))
    if (!dl)
        stop("dim(X) must have a positive length")
    if (is.object(X))
        X <- if (dl == 2L)
            as.matrix(X)
        else as.array(X)
    d <- dim(X)
    dn <- dimnames(X)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        if (is.null(dnn <- names(dn)))
            stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    if (d2 == 0L) {
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call),
            1L))
        ans <- forceAndCall(1, FUN, if (length(d.call) < 2L) newX[,
            1] else array(newX[, 1L], d.call, dn.call), ...)
        return(if (is.null(ans)) ans else if (length(d.ans) <
            2L) ans[1L][-1L] else array(ans, d.ans, dn.ans))
    }
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)

    if (is.null(cl)) { # sequential follows base::apply

        pb <- startpb(0, d2) # pb_specific_code

        if (length(d.call) < 2L) {
            if (length(dn.call))
                dimnames(newX) <- c(dn.call, list(NULL))
            for (i in 1L:d2) {
                tmp <- forceAndCall(1, FUN, newX[, i], ...)
                if (!is.null(tmp))
                    ans[[i]] <- tmp

                setpb(pb, i) # pb_specific_code

            }
        }
        else for (i in 1L:d2) {
            tmp <- forceAndCall(1, FUN, array(newX[, i], d.call,
                dn.call), ...)
            if (!is.null(tmp))
                ans[[i]] <- tmp

            setpb(pb, i) # pb_specific_code

        }

        closepb(pb) # pb_specific_code

        ans.list <- is.recursive(ans[[1L]])
        l.ans <- length(ans[[1L]])
        ans.names <- names(ans[[1L]])
        if (!ans.list)
            ans.list <- any(unlist(lapply(ans, length)) != l.ans)
        if (!ans.list && length(ans.names)) {
            all.same <- vapply(ans, function(x) identical(names(x),
                ans.names), NA)
            if (!all(all.same))
                ans.names <- NULL
        }

    } else { # parallel follows parallel::parApply

        arglist <- if (length(d.call) < 2L) {
            if (length(dn.call))
                dimnames(newX) <- c(dn.call, list(NULL))
            lapply(seq_len(d2), function(i) newX[, i])
        }
        else lapply(seq_len(d2), function(i) array(newX[, i], d.call,
            dn.call))
        #ans <- parallel::parLapply(cl = cl, X = arglist, fun = FUN, ...)
        ## rely on pblapply for calling parLapply with pb
        ans <- pblapply(X = arglist, FUN = FUN, ..., cl = cl)
        ans.list <- is.recursive(ans[[1L]])
        l.ans <- length(ans[[1L]])
        ans.names <- names(ans[[1L]])
        if (!ans.list)
            ans.list <- any(lengths(ans) != l.ans)
        if (!ans.list && length(ans.names)) {
            all.same <- vapply(ans, function(x) identical(names(x),
                ans.names), NA)
            if (!all(all.same))
                ans.names <- NULL
        }

    } # end of parallel portion

    len.a <- if (ans.list)
        d2
    else length(ans <- unlist(ans, recursive = FALSE))
    if (length(MARGIN) == 1L && len.a == d2) {
        names(ans) <- if (length(dn.ans[[1L]]))
            dn.ans[[1L]]
        return(ans)
    }
    if (len.a == d2)
        return(array(ans, d.ans, dn.ans))
    if (len.a && len.a%%d2 == 0L) {
        if (is.null(dn.ans))
            dn.ans <- vector(mode = "list", length(d.ans))
        dn1 <- if (length(dn.call) && length(ans.names) == length(dn.call[[1L]]))
            dn.call[1L]
        else list(ans.names)
        dn.ans <- c(dn1, dn.ans)
        return(array(ans, c(len.a%/%d2, d.ans), if (!is.null(names(dn.ans)) ||
            !all(vapply(dn.ans, is.null, NA))) dn.ans))
    }
    return(ans)
}
