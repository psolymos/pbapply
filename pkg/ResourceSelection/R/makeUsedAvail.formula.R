makeUsedAvail.formula <- 
function(formula, data = parent.frame(), ...)
{
    mf <- match.call(expand.dots = FALSE)
    mm <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, mm)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    x <- eval(mf, parent.frame())
    rid <- attr(terms(x), "response")
    YNAME <- colnames(x)[rid]
    y <- model.response(x, "numeric")
    x <- data.frame(x[,-rid])
    eval(parse(text=paste(YNAME, "<-y", sep="")))
    eval(parse(text=paste("makeUsedAvail.default(x=x, y=",YNAME,", m=m, ...)", sep="")))
}

