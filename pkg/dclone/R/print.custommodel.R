print.custommodel <-
function (x, ...)
{
    y <- x
    attributes(y) <- NULL
#    print(paste(as.character(Call[2]), " <- "))
    dput(y)
    invisible(x)
}
custommodel(jfun)