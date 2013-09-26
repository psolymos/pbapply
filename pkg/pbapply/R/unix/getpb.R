getpb <-
function(pb)
{
    if (dopb()) {
        progress.bar <- getOption("pboptions")$type
        rval <- switch(progress.bar, 
            txt = getTxtProgressBar(pb), 
            tk = tcltk::getTkProgressBar(pb))
    } else rval <- NULL
    rval
}

