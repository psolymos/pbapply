print.custommodel <-
function (x, deparse = FALSE, ...)
{
    if (deparse)
        dput(x, control=NULL) else print.default(x, ...)
    invisible(x)
}
