model.matrix.rsf <-
function (object, ...)
{
    out <- model.matrix.default(object, ...)
    if (object$link == "log")
        out <- out[,-1]
    out
}

