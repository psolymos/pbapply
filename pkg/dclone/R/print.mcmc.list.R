print.mcmc.list <-
function(x, ...)
{
    y <- x
    attributes(y) <- NULL
    print(y)
    invisible(x)
}
