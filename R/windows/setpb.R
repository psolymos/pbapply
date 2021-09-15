setpb <-
function(pb, value)
{
    if (dopb()) {
        control <- getOption("pboptions")
        rval <- switch(control$type,
            timer = setTxtProgressBar(pb, value),
            txt = setTxtProgressBar(pb, value),
            win = setWinProgressBar(pb, value, label = control$label),
            tk = tcltk::setTkProgressBar(pb, value, label = control$label))
        if (doshiny())
            rval <- pb$set(value,
                message = if (control$title == "") NULL else control$title,
                detail = if (control$label == "") NULL else control$label)
    } else {
        rval <- NULL
    }
    invisible(rval)
}

