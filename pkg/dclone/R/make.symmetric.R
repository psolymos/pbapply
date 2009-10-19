make.symmetric <-
function(x)
{
    if (dim(x)[1] != dim(x)[2])
        stop("'x' is not sqare matrix")
    rval <- t(x)
    m <- (x[lower.tri(x)] + rval[lower.tri(rval)]) / 2
    rval[lower.tri(rval)] <- m
    rval <- t(rval)
    rval[lower.tri(rval)] <- m
    rval
}

