dcdim <-
function(x, drop=TRUE)
{
    if (!is.null(dim(x)))
        attr(x, "drop") <- drop
    class(x) <- c("dcdim", class(x))
    x
}
