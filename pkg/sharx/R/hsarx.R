hsarx <- 
function(formula, data, n.clones, cl=NULL, subset, na.action, ...)
{
    if (missing(n.clones))
        stop("'n.clones' argument missing")
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    f <- Formula(formula)
    st <- length(f)
    if (st[1] != 1)
        stop("multiple responses in LHS are not allowed")
    mf[[1]] <- as.name("model.frame")
    mf$formula <- f
    mf <- eval(mf, parent.frame())
    Y <- model.response(mf)
    X <- model.matrix(f, data = mf, rhs = 1)
    if (st[2] > 1) {
        Z <- model.matrix(f, data = mf, rhs = 2)
        if (length(formula(f, lhs=FALSE, rhs=3)[[2]]) > 1)
            stop("inappropriate grouping variable")
        G <- model.matrix(f, data = mf, rhs = 3)
        if (ncol(G) > 2) {
            G[rowSums(G[,-1]) != 0,1] <- 0
            G <- rowSums(col(G) * G)
        } else {
            G <- G[,2]
        }
        G <- as.integer(as.factor(G))
        if (length(unique(G)) == 1)
            stop("grouping variable must have at least 2 levels")

if (ncol(X) > 2)
    stop("multiple focal covariates not supported")

    } else {
        Z <- NULL
        G <- NULL
    }
    out <- hsarx.fit(Y, X, Z, G, n.clones, cl, ...)
    out
}
