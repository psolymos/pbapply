svisitFormula <-
function(formula, data=NULL, n=0)
{
    if (is.null(data))
        data <- sys.frame(n)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]], as.name("|"))) {
        ff <- formula
        formula[[3]][1] <- call("+")
        mf$formula <- formula
        ffsta <- . ~ .
        ffdet <- ~ .
        ffsta[[2]] <- ff[[2]]
        ffsta[[3]] <- ff[[3]][[2]]
        ffdet[[3]] <- ff[[3]][[3]]
        ffdet[[2]] <- NULL
    } else {
        stop("detection part of the formula is empty")
    }
    if (any(sapply(unlist(as.list(ffdet[[2]])), function(x) identical(x, 
        as.name("."))))) {
        ffdet <- eval(parse(text = sprintf(paste("%s -", deparse(ffsta[[2]])), 
            deparse(ffdet))))
    }
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- terms(formula, "zif", data = data)
    mtX <- terms(ffsta, "zif", data = data)
    if (!is.null(attr(mtX, "specials")$zif)) {
        tlabs <- attr(terms(ffsta),"term.labels")
        id <- tlabs %in% rownames(attr(mtX, "factors"))[attr(mtX, "specials")$zif]
        fftmp1 <- paste(tlabs[!id], collapse = " + ")
        fftmp2 <- paste(tlabs[id], collapse = " + ")
        ffzif <- as.formula(paste(deparse(ffsta[[2]]), deparse(ffsta[[1]]), fftmp2, collapse = " "))
        ffsta <- as.formula(paste(deparse(ffsta[[2]]), deparse(ffsta[[1]]), fftmp1, collapse = " "))
        mtQ <- terms(ffzif, "zif", data = data)
        mtX <- terms(ffsta, "zif", data = data)
        X <- model.matrix(mtX, mf)
        Q <- model.matrix(mtQ, mf)
        cn <- colnames(Q)
        cn <- gsub("(.*)(zif\\()", "\\1", cn)
        cn[-1] <- gsub("(.*)(\\))", "\\1", cn[-1])
        colnames(Q) <- cn
    } else {
        mtQ <- NULL
        ffzif <- NULL
        X <- model.matrix(mtX, mf)
        Q <- NULL
    }
    mtZ <- terms(ffdet, data = data)
    mtZ <- terms(update(mtZ, ~.), data = data)
    Z <- model.matrix(mtZ, mf)
    Y <- model.response(mf, "numeric")
    Xlevels <- .getXlevels(mt, mf)

    repl.id <- which(substr(colnames(mf), 1, 4) == "zif(")
    repl.value <- substr(colnames(mf)[repl.id], 5, nchar(colnames(mf)[repl.id]) - 1)
    colnames(mf)[repl.id] <- repl.value

    out <- list(y=Y,
        formula=list(sta = ffsta, det = ffdet, zif = ffzif, full = ff),
        terms=list(sta=mtX, det=mtZ, zif=mtQ, full=mt),
        levels=Xlevels,
        contrasts=list(sta = attr(X, "contrasts"), det = attr(Z, "contrasts"), sta = attr(Q, "contrasts")),
        model=mf,
        x=list(sta = X, det = Z, zif = Q))
    out
}

