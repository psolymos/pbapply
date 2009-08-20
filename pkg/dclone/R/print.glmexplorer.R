print.glmexplorer <-
function(x, ...)
{
    attributes(x) <- NULL
    class(x) <- "list"
    print(x)
    invisible(x)
}
