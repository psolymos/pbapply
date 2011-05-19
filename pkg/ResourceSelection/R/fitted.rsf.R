fitted.rsf <-
function(object, type=c("all", "used", "avail"), ...)
{
    type <- match.arg(type)
    switch(type,
        all = object$fitted.values,
        used = object$fitted.values[object$y==1],
        avail = object$fitted.values[object$y==0])
}

