predict.svocc <-
function(object, newdata = NULL, type = c("link", "response"), 
model = c("sta", "det"), se.fit = FALSE, ...){
    type <- match.arg(type)
    model <- match.arg(model)
    if (se.fit) {
        boot <- attr(object, "bootstrap")
        if (is.null(boot))
            stop("cannot provide prediction Std Error without Bootstrap")
        Bp <- ncol(boot)
        nps <- sapply(object$coefficients, length)
        id <- switch(model,
            sta = 1:nps[1],
            det = (nps[1]+1):sum(nps[1:2]))
    }
    if (is.null(newdata)) {
        rval <- switch(model,
            sta = object$occurrence.probabilities,
            det = object$detection.probabilities)
        if (type == "link")
            rval <- binomial(object$link[[model]])$linkfun(rval)
        if (se.fit)
            X <- model.matrix(object, model)
    } else {
        rhs <- model.frame(object$formula[[model]], newdata)
        X <- model.matrix(attr(rhs, "terms"), rhs)
        rval <- drop(X %*% coef(object, model))
        if (type == "response")
            rval <- binomial(object$link[[model]])$linkinv(rval)
    }
    if (se.fit) {
        se <- sapply(1:Bp, function(i) drop(X %*% boot[id,i]))
        if (type == "response")
            se <- binomial(object$link[[model]])$linkinv(se)
        se <- apply(se, 1, sd)
        rval <- list(fit = rval, se.fit = se)
    }
    rval
}
