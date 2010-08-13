hsar <- 
function(formula, data, group, n.clones=1, subset, na.action, ...)
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
        gnam <- as.character(formula(f, lhs=FALSE, rhs=3)[[2]])
        if (length(gnam) > 1)
            stop("provide only one grouping variable")
        G <- mf[,gnam]
        G <- as.numeric(G)
        if (min(G) > 1 || max(G) > length(Y))
            stop("inappropriate grouping variable")
    } else {
        Z <- NULL
        G <- NULL
    }
    out <- hsar.fit(Y, X, Z, G, n.clones, ...)
    class(out) <- "hsar"
    out$formula <- f
    out
}

hsar.fit <- 
function(Y, X, Z, G, n.clones, ...)
{
    list(Y=Y, X=X, Z=Z, G=G, n.clones=n.clones)
}

library(Formula)
d <- data.frame(S=c(1:10, 10:1), A=c(1:10, 10:1) / 10, study=rep(c("b","a"), each=10))
db <- data.frame(study=c("a","b"), H=c(100,200))
dat <- data.frame(d, db[match(d$study, db$study),])

x <- hsar(S ~ A, dat)
x <- hsar(S ~ A | H | study, dat)

http://cran.r-project.org/web/packages/Formula/vignettes/Formula.pdf