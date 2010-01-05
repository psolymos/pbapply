## this implementation includes only optim version
`singleocc` <-
function (formula, data, link = "logit", penalized = FALSE,
    method = c("optim", "dc"), n.clones = 1000, 
    subset, na.action = na.omit, model = TRUE, x = FALSE, ...)
{
    ## evaluate formula and data
    if (missing(data))
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]], 
        as.name("|"))) {
        ff <- formula
        formula[[3]][1] <- call("+")
        mf$formula <- formula
        ffocc <- . ~ .
        ffdet <- ~.
        ffocc[[2]] <- ff[[2]]
        ffocc[[3]] <- ff[[3]][[2]]
        ffdet[[3]] <- ff[[3]][[3]]
        ffdet[[2]] <- NULL
    }
    else {
        stop("detection part of the formula is empty")
    }
    if (any(sapply(unlist(as.list(ffdet[[2]])), function(x) identical(x, 
        as.name("."))))) {
        ffdet <- eval(parse(text = sprintf(paste("%s -", deparse(ffocc[[2]])), 
            deparse(ffdet))))
    }
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- terms(formula, data = data)
    mtX <- terms(ffocc, data = data)
    X <- model.matrix(mtX, mf)
    mtZ <- terms(ffdet, data = data)
    mtZ <- terms(update(mtZ, ~.), data = data)
    Z <- model.matrix(mtZ, mf)
    Y <- model.response(mf, "numeric")
    if (length(Y) < 1) 
        stop("empty model")
    if (all(Y > 0)) 
        stop("invalid dependent variable, no zero value")
    if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y + 
        0.001))))) 
        stop("invalid dependent variable, non-integer values")
    Y <- as.integer(round(Y + 0.001))
    if (any(Y < 0)) 
        stop("invalid dependent variable, negative counts")
    if (any(!(Y %in% c(0, 1)))) 
        stop("invalid dependent variable, not in c(0, 1)")
    if (setequal(colnames(Z), colnames(X))) 
        stop("at least one covariate should be separate for occupancy and detection parts of the formula")
    if (length(Y) != NROW(X)) 
        stop("invalid dependent variable, not a vector")
    Xlevels <- .getXlevels(mt, mf)
    if (all(union(colnames(X), colnames(Z))[-1] %in% names(unlist(Xlevels))))
        stop("model must include at least one continuous covariate")

    ## occ and det linkinv functions
    if (length(link) == 1)
        link <- rep(link, 2)
    links <- c("logit", "probit", "cloglog")
    link.occ <- match.arg(link[1], links)
    link.det <- match.arg(link[2], links)
    ## method
    method <- match.arg(method)
    ## fit
    out <- singleocc.fit(Y, X, Z, link.occ, link.det, penalized = penalized, auc = FALSE, 
        method=method, n.clones=n.clones, prec=0.1, ...)
#    if (inherits(out, "try-error"))
#        return(out)
    out$call = match.call()
    out$formula <- list(occ = ffocc, det = ffdet, full = ff)
    out$terms <- list(occ = mtX, det = mtZ, full = mt)
    out$levels <- Xlevels
    out$contrasts <- list(occ = attr(X, "contrasts"), det = attr(Z, "contrasts"))
    if (model) 
        out$model <- mf
    if (x) 
        out$x <- list(occ = X, det = Z)
    class(out) <- c("singleocc")
    return(out)
}
