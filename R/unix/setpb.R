setpb <-
function(pb, value)
{
    if (dopb()) {
        control <- getOption("pboptions")
        rval <- switch(control$type,
            custom = { pb(); pb },
            txt = setTxtProgressBar(pb, value), 
            tk = tcltk::setTkProgressBar(pb, value, label=control$label))
    } else rval <- NULL
    invisible(rval)
}

