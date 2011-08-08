coef.summary.svabu <-
function(object, ...)
{
    x <- object
    sta <- array(t(x$sta))
    det <- array(t(x$det))
    zif <- array(x$zif)
    out <- matrix(c(sta, det, zif), nrow(x$sta) + nrow(x$det) + nrow(x$zif), 4, byrow = TRUE)
    rownames(out) <- c(paste("sta", rownames(x$sta), sep = "_"),
            paste("det", rownames(x$det), sep = "_"),
            paste("zif", rownames(x$zif), sep = "_"))
    colnames(out) <- colnames(x$sta)
    return(out)
}
