startpb <-
function(min=0, max=1)
{
    if (dopb()) {
        progress.bar <- getOption("pbapply.pb")
        control <- switch(progress.bar,
            txt = getOption("pbapply.txt"),
            win = getOption("pbapply.gui"),
            tk = getOption("pbapply.gui"))
        pb <- switch(progress.bar, 
            txt = txtProgressBar(min, max, initial=control$initial,
                style = control$style, width = control$width, char = control$char),
            win = winProgressBar(min=min, max=max, initial=control$initial,
                title = control$title, label = control$label),
            tk = tkProgressBar(min=min, max=max, initial=control$initial,
                title = control$title, label = control$label))
    } else pb <- NULL
    invisible(pb)
}

