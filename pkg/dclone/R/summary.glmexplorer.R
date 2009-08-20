summary.glmexplorer <-
function(object, crit=0.05, ...)
{
    if (attr(object, "select")) {
        est <- lapply(object, function(z) {
            tmp <- z[,1]
            names(tmp) <- rownames(z)
            tmp})
        nam <- colnames(attr(object, "model"))
        rval <- matrix(NA, length(nam), length(est),
            dimnames=list(nam, names(est)))
        for (i in 1:length(est)) {
            id <- match(rownames(object[[i]]), nam)
            rval[id,i] <- est[[i]]
        }
    } else {
        rval <- as.data.frame(lapply(object, function(z) z[,1]))
        rownames(rval) <- rownames(object[[1]])
        pval <- as.data.frame(lapply(object, function(z) z[,4]))
        rval <- data.matrix(rval)
        pval <- data.matrix(pval)
        if (rownames(object[[1]])[1] == "(Intercept)")
            pval[1,] <- 0
        rval[pval >= crit] <- NA
    }
    id <- lapply(1:ncol(rval), function(z) {
        row(rval)[,z][!is.na(rval[,z])]})
    names(id) <- colnames(rval)
    class(rval) <- c("summary.glmexplorer", "matrix")
    attr(rval, "id") <- id
    rval
}
