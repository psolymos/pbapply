setpb <-
function(pb, value)
{
    if (dopb()) {
        progress.bar <- getOption("pbapply.pb")
        control <- switch(progress.bar,
            txt = getOption("pbapply.txt"),
            win = getOption("pbapply.gui"),
            tk = getOption("pbapply.gui"))
        rval <- switch(progress.bar, 
            txt = setTxtProgressBar(pb, value), 
            win = setWinProgressBar(pb, value, label=control$label),
            tk = tcltk:::setTkProgressBar(pb, value, label=control$label))
    } else rval <- NULL
    invisible(rval)
}

