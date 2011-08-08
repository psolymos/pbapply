AUC.svocc <-
function(object, ...)
{
    AUCFUN <- function(x) {
        if (is.null(x$auc)) {
            zp <- (x$occurrence.probabilities * x$detection.probabilities)[x$y == 0]
            op <- (x$occurrence.probabilities * x$detection.probabilities)[x$y == 1]
            u <- sum(unlist(lapply(op, function(z) sum(zp < z))))
            out <- u / (sum(x$y) * (x$nobs - sum(x$y)))
        } else {
            out <- x$auc
        }
        return(out)
    }
    if (nargs() > 1) {
        object <- list(object, ...)
        val <- data.frame(AUC=sapply(object, AUCFUN))
        row.names(val) <- as.character(match.call()[-1])
        return(val)
    }
    AUCFUN(object)
}

