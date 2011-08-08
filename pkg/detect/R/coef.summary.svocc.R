coef.summary.svocc <-
function(object, ...)
{
    x <- object
    sta <- array(t(x$sta))
    det <- array(t(x$det))
    out <- matrix(c(sta, det), nrow(x$sta) + nrow(x$det), 4, byrow = TRUE)
    rownames(out) <- c(paste("sta", rownames(x$sta), sep = "_"),
            paste("det", rownames(x$det), sep = "_"))
    colnames(out) <- colnames(x$sta)
    return(out)
}

