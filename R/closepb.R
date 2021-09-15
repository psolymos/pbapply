closepb <-
function(pb)
{
    if (is.null(pb)) {
        invisible(NULL)
    } else {
        if (doshiny()) {
            pb$close()
        } else {
            close(pb)
        }
    }
}

