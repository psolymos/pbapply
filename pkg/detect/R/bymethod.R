bymethod <- 
function(Y, D, X=NULL) 
{
    D0 <- D
    D0[is.na(D0)] <- -1
    Dlist <- lapply(1:NCOL(D), function(i) as.factor(D0[,i]))
    Dtype <- interaction(Dlist, drop=TRUE)
    id <- !duplicated(Dtype)
    D0 <- matrix(D[id,], sum(id), NCOL(D))
    X0 <- if (is.null(X))
        NULL else matrix(X[id,], sum(id), NCOL(X))
    Y0 <- aggregate(Y, list(Dtype), sum, na.rm=TRUE)
    Y0 <- Y0[match(Dtype[id], Y0[,1]),]
    Y0 <- as.matrix(Y0[,-1])
    Y0[is.na(D0)] <- NA
    rownames(Y0) <- NULL
    colnames(Y0) <- colnames(Y)
    rownames(D0) <- NULL
    colnames(D0) <- colnames(D)
    if (!is.null(X)) {
        rownames(X0) <- NULL
        colnames(X0) <- colnames(X)
    }
    list(Y=Y0, D=D0, X=X0)
}
