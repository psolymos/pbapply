pblapply <-
function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    progress.bar <- getOption("pbapply.pb")
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, c("txt", "win", "tk", "none"))
        if (progress.bar == "none") 
            progress.bar <- NULL
    }
    B <- length(X)
    do.pd <- interactive() && !is.null(progress.bar) && B >= 1
    if (!do.pd)
        return(.Internal(lapply(X, FUN)))
    control <- switch(progress.bar,
        text = getOption("pbapply.txt"),
        win = getOption("pbapply.gui"),
        tk = getOption("pbapply.gui"))
    pb <- switch(progress.bar, 
        text = txtProgressBar(0, B, initial=control$initial,
            style = control$style, width = control$width, char = control$char),
        win = winProgressBar(min=0, max=B, initial=control$initial,
            title = control$title, label = control$label),
        tk = tkProgressBar(min=0, max=B, initial=control$initial,
            title = control$title, label = control$label))
    rval <- vector("list", length(X))
    for (i in 1:B) {
        rval[[i]] <- FUN(X[[i]], ...)
        switch(progress.bar, 
            text = setTxtProgressBar(pb, i), 
            win = setWinProgressBar(pb, i, label=control$label),
            win = setTkProgressBar(pb, i, label=control$label))
    }
    close(pb)
    rval
}

