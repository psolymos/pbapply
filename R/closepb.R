closepb <-
function(pb)
{
    #if (is.null(pb) || getOption("pboptions")$type == "timer")
    if (is.null(pb))
        invisible(NULL) else close(pb)
}

