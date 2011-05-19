BIC.rsf <-
function(object, ...)
{
    if (nargs() > 1) {
        object <- list(object, ...)
        val <- lapply(object, logLik)
        val <- as.data.frame(t(sapply(val, function(el) c(attr(el, "df"), nlme:::BIC.logLik(el)))))
        names(val) <- c("df", "BIC")
        row.names(val) <- as.character(match.call()[-1])
        return(val)
    }
    BIC(logLik(object))
}

