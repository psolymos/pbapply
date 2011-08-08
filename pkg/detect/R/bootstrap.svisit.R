bootstrap.svisit <-
function(object, B, type=c("nonpar", "param"), seed=NULL, ...) {
    type <- match.arg(type)
    CALL <- object$call
    CALL$data <- as.name("mfi")
    ini <- coef(object)
    ## use estimates as initial values
    CALL$inits <- as.name("ini")
    ## svabu needs phi.boot=0 -- not using bootstrap any more
#    if (inherits(object, "svabu"))
#        CALL$phi.boot <- 0
    mf <- model.frame(object)
    n <- object$nobs
    ## archiving global env
    if (exists("mfi", envir=parent.frame())) {
        assign("tmp1", get("mfi", envir=parent.frame()))
#        on.exit(rm(list="mfi", envir=parent.frame()), add=TRUE)
        on.exit(assign("mfi", tmp1, envir=parent.frame()), add=TRUE)
    }
    if (exists("ini", envir=parent.frame())) {
        assign("tmp2", get("ini", envir=parent.frame()))
        on.exit(assign("ini", tmp2, envir=parent.frame()), add=TRUE)
    }
    assign("ini", ini, envir=parent.frame())
    if (type == "nonpar") {
        if (!is.null(seed))
            set.seed(seed)
        b <- lapply(1:B, function(i) sample(1:n, n, replace=TRUE))
        bfun <- function(i) {
            assign("mfi", mf[i,], envir=parent.frame())
            mod <- eval(CALL, envir=parent.frame())
            ## here add more params
            coef(mod)
        }
    } else {
        mfi <- mf
        rid <- attr(attr(mf, "terms"), "response")
        sim <- simulate(object, B, seed)
        b <- 1:B
        bfun <- function(i) {
            mf[,rid] <- sim[,i]
            assign("mfi", mf, envir=parent.frame())
            mod <- eval(CALL, envir=parent.frame())
            ## here add more params
            coef(mod)
        }
    }
    ## doing bootstrap
    rval <- if (require(pbapply))
        pblapply(b, bfun) else lapply(b, bfun)
    rm(list="ini", envir=parent.frame())
    ## making rval
    rval <- matrix(unlist(rval), length(rval[[1]]), B)
    rval <- cbind(coef(object), rval)
    attr(rval, "type") <- type
    attr(rval, "ini") <- ini
    attr(object, "bootstrap") <- rval
    object
}

