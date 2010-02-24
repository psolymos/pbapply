print.custommodel <-
function (x, ...)
{
    dput(x, control=NULL)
    invisible(x)
}
