closepb <-
function(pb)
{
    if (is.null(pb) || getOption("pboptions")$type == "timer")
        invisible(NULL) else close(pb)
}

