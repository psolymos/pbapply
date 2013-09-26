## this creates the model matrices from formula
## Y | D ~ Q | X | Z
dFormula <- 
function(formula, data, drop=TRUE, count=TRUE, ...)
{
#    require(Formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    f <- Formula(formula)
    mf[[1]] <- as.name("model.frame")
    mf$formula <- f
    mf$na.action <- na.pass
    mf <- eval(mf, parent.frame())
#    mf <- eval(mf, environment(formula))
    st <- length(f)
    if (!(st[1] %in% 1:2))
        stop("LHS ill-defined")
    if (!(st[2] %in% 1:3))
        stop("RHS ill-defined")
    if (st[1] == 1) {
        Y <- model.response(mf)
        Y <- unname(Y)
        Y <- drop(Y)
        if (NCOL(Y) > 1)
            stop("response matrix not allowed without methodology")
        D <- NULL
    } else {
        if (st[1] >2)
            stop(">2 parts in LHS not supported")
        Y <- drop(data.matrix(model.part(f, data=mf, lhs=1)[[1]]))
        D <- drop(data.matrix(model.part(f, data=mf, lhs=2)[[1]]))
        if (NCOL(Y) != NCOL(D))
            stop("ncol must be equal in response and methodology")
    }
    if (st[2] == 1) {
        X <- model.matrix(f, data = mf, rhs = 1)
        Z <- NULL
        Q <- NULL
    }
    if (st[2] == 2) {
        X <- model.matrix(f, data = mf, rhs = 1)
        Z <- model.matrix(f, data = mf, rhs = 2)
        Q <- NULL
#        st[2] <- 0
    }
    if (st[2] == 3) {
        Q <- model.matrix(f, data = mf, rhs = 1)
        X <- model.matrix(f, data = mf, rhs = 2)
        Z <- model.matrix(f, data = mf, rhs = 3)
#        st[2] <- 1
    }
    checkDesign(Y, D, X, Z, Q, count)
    if (!drop && is.null(dim(Y))) {
        Y <- data.matrix(Y)
        if (!is.null(D))
            D <- data.matrix(D)
    }
    list(Y=Y,D=D,Q=Q,X=X,Z=Z,model=mf,formula=f,
        nobs=NROW(Y), 
        df.residual=NROW(Y) - NCOL(X))
}
