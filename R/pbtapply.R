pbtapply <-
function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE,
          cl = NULL) # changed here
{
    FUN <- if (!is.null(FUN))
        match.fun(FUN)
    if (!is.list(INDEX))
        INDEX <- list(INDEX)
    INDEX <- lapply(INDEX, as.factor)
    nI <- length(INDEX)
    if (!nI)
        stop("'INDEX' is of length zero")
    if (!all(lengths(INDEX) == length(X)))
        stop("arguments must have same length")
    namelist <- lapply(INDEX, levels)
    extent <- lengths(namelist, use.names = FALSE)
    cumextent <- cumprod(extent)
    if (cumextent[nI] > .Machine$integer.max)
        stop("total number of levels >= 2^31")
    storage.mode(cumextent) <- "integer"
    ngroup <- cumextent[nI]
    group <- as.integer(INDEX[[1L]])
    if (nI > 1L)
        for (i in 2L:nI) group <- group + cumextent[i - 1L] *
            (as.integer(INDEX[[i]]) - 1L)
    if (is.null(FUN))
        return(group)
    levels(group) <- as.character(seq_len(ngroup))
    class(group) <- "factor"
    ans <- split(X, group)
    names(ans) <- NULL
    index <- as.logical(lengths(ans))
    ans <- pblapply(X = ans[index], FUN = FUN, ..., cl = cl) # changed here
    ansmat <- array(if (simplify && all(lengths(ans) == 1L)) {
        ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
        if (!is.null(ans) && is.na(default) && is.atomic(ans))
            vector(typeof(ans))
        else default
    }
    else vector("list", prod(extent)), dim = extent, dimnames = namelist)
    if (length(ans)) {
        ansmat[index] <- ans
    }
    ansmat
}

