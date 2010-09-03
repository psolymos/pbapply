pblapply <-
function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    progress.bar <- getOption("pbapply.pb")
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, c("txt", 
            "tk", "none"))
        if (progress.bar == "none") 
            progress.bar <- NULL
    }
    B <- length(X)
    do.pd <- interactive() && !is.null(progress.bar) && B >= 1
    if (!do.pd) 
        return(.Internal(lapply(X, FUN)))
    control <- switch(progress.bar, 
        txt = getOption("pbapply.txt"), 
        tk = getOption("pbapply.gui"))
    pb <- switch(progress.bar, 
        txt = txtProgressBar(0, B, initial = control$initial, 
            style = control$style, width = control$width, char = control$char), 
        tk = tcltk:::tkProgressBar(min = 0, 
            max = B, initial = control$initial, title = control$title, 
            label = control$label))
    rval <- vector("list", length(X))
    for (i in 1:B) {
        rval[[i]] <- FUN(X[[i]], ...)
        switch(progress.bar, 
            txt = setTxtProgressBar(pb, i), 
            tk = tcltk:::setTkProgressBar(pb, i, label = control$label))
    }
    close(pb)
    rval
}

