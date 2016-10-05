closepb <-
function(pb)
{
    if (is.null(pb))
        invisible(NULL) else close(pb)
}

