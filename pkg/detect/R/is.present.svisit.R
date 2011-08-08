is.present.svisit <-
function (object, n.imp = 0, ...)
{
    if (inherits(object, "svocc")) {
        rval <- fitted(object)
    } else {
        z1 <- 1 - object$zif.probabilities
        p1 <- 1 - exp(-fitted(object))
        obs01 <- ifelse(object$y > 0, 1, 0)
        rval <- pmax(obs01, z1 * p1)
    }
    if (n.imp) {
        rval <- rbinom(length(object$y) * n.imp, 1, rval)
        dim(rval) <- c(length(object$y), n.imp)
        rownames(rval) <- case.names(object)
        colnames(rval) <- paste("imp", 1:n.imp, sep=".")
    } else names(rval) <- case.names(object)
    rval
}
