`coef.summary.singleocc` <-
function(object, ...)
{
    x <- object
    occ <- array(t(x$occ))
    det <- array(t(x$det))
    out <- matrix(c(occ, det), nrow(x$occ) + nrow(x$det), 4, byrow = TRUE)
    rownames(out) <- c(paste("occ", rownames(x$occ), sep = "_"),
            paste("det", rownames(x$det), sep = "_"))
    colnames(out) <- colnames(x$occ)
    return(out)
}
