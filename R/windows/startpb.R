startpb <-
function(min=0, max=1)
{
    if (dopb()) {
        control <- getOption("pboptions")
        pb <- switch(control$type, 
            txt = txtProgressBar(min, max, initial=control$initial,
                style = control$style, width = control$txt.width, char = control$char),
            win = winProgressBar(min=min, max=max, initial=control$initial,
                title = control$title, label = control$label, width = control$gui.width),
            tk = tcltk::tkProgressBar(min=min, max=max, initial=control$initial,
                title = control$title, label = control$label, width = control$gui.width))
    } else pb <- NULL
    invisible(pb)
}
