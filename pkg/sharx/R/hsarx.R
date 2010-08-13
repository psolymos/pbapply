#http://cran.r-project.org/web/packages/Formula/vignettes/Formula.pdf
hsarx <- 
function(formula, data, group, n.clones=1, cl=NULL, subset, na.action, ...)
{
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
    } else {
        Z <- NULL
        G <- NULL
    }
    out <- hsarx.fit(Y, X, Z, G, n.clones, cl, ...)
    class(out) <- "hsar"
    out$formula <- f
    out$model <- mf
    out
}

hsarx.fit <- 
function(Y, X, Z, G, n.clones=1, cl=NULL, ...)
{
    list(Y=Y, X=X, Z=Z, G=G, n.clones=n.clones, cl=cl)
}

library(Formula)
d <- data.frame(S=c(1:10, 10:1), A=c(1:10, 10:1) / 10, study=rep(c("b","a"), each=10))
db <- data.frame(study=c("a","b"), H=c(100,200))
dat <- data.frame(d, db[match(d$study, db$study),])

x <- hsarx(S ~ A, dat)
x <- hsarx(S ~ A | H | study, dat)
x$G
