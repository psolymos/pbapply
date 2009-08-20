glmexplorer <-
function(formula, data, family=poisson(), select=FALSE, ...)
{
    .formula <- formula
    lhs <- formula[[2]]
    if (missing(data))
        data <- parent.frame()
    Y <- as.matrix(eval(lhs, data))
    formula[[2]] <- NULL
    rhs <- model.frame(formula, data, drop.unused.levels = TRUE)
    X <- model.matrix(attr(rhs,"terms"), rhs)
    if (nrow(Y) != nrow(X))
        stop("number of rows in Y and X must conform")
    if (is.na(Y) || is.na(X))
        stop("'NA' values not allowed")
    n <- NROW(Y)
    J <- NCOL(Y)
    if (select) {
        glmfit <- lapply(1:J, function(z) try(glm(Y[, z] ~ ., rhs, family=family)))
        glmsel <- lapply(glmfit, function(z) try(step(z, direction="backward", trace=0)))
        for (i in 1:length(glmsel)) {
            if (!inherits(glmsel[[i]], "try-error")) {
                glmfit[[i]] <- glmsel[[i]]
            } else {
                attr(glmfit[[i]], "try-error") <- glmsel[[i]]
            }
        }
        rval <- lapply(glmfit, function(z) try(summary(z)$coef))
        for (i in 1:length(rval))
            attr(rval[[i]], "converged") <- glmfit[[i]]$converged
    } else {
        glmfit <- lapply(1:J, function(z) try(glm.fit(X, Y[, z], family=family)))
        rval <- lapply(glmfit, function(z) {
            class(z) <- "glm"
            try(summary(z)$coef)})
        for (i in 1:length(rval))
            attr(rval[[i]], "converged") <- glmfit[[i]]$converged
    }
    if (!is.null(colnames(Y)))
        names(rval) <- colnames(Y)
    class(rval) <- "glmexplorer"
    attr(rval, "family") <- family$family
    attr(rval, "formula") <- .formula
    attr(rval, "select") <- select
    attr(rval, "y") <- Y
    attr(rval, "model") <- X
    rval
}
