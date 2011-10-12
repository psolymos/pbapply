setClass("sie", 
    contains = "mle",
    representation(S="numeric", A="numeric"))

sie <- 
function(S, A, method = "Nelder-Mead", ...) 
{
    require(stats4)
    sietfun <- function(T) {
        x <- (log(A)-T) * (log(A) >= T)
        -logLik(lm(log(S+0.5)~x))
    }
    That <- optimize(sietfun, interval=range(log(A)))$minimum
    IndlogA_T <- (log(A)-That) * (log(A) >= That)
    m <- lm(log(S+0.5) ~ IndlogA_T)
    cfs <- c(m$coefficients, That)
    names(cfs) <- NULL
    inits <- list(logc=cfs[1], z=cfs[2], T=cfs[3])
    N <- length(S)
    p <- 3
    w <- rep.int(1, N)
    nll <- function(logc, z, T) {
        res <- log(S+0.5) - (logc + (log(A) >= T)*z*(log(A)-T))
        -0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))
    }
    res <- stats4:::mle(nll, start=inits, method=method, nobs=length(S), ...)
    attr(res, "data") <- cbind(A, S)
    res@call <- match.call()
    res <- as(res, "sie")
    res@S <- as.numeric(S)
    res@A <- as.numeric(A)
    res
}

sieplot <- 
function(x, add = FALSE, ...) 
{
    if (!inherits(x, "sie"))
        stop("'sie' class expected")
    logA <- log(x@A)
    logS <- log(x@S + 0.5)
    cfs <- coef(x)
    xx <- c(min(logA), cfs["T"], max(logA))
    yy <- c(cfs["logc"], cfs["logc"], cfs["logc"]+cfs["z"]*(max(logA)-cfs["T"]))
    names(xx) <- names(yy) <- NULL
    if (!add)
        plot(logA, logS, xlab="log(A)", ylab="log(S+0.5)", ylim=range(logS, yy), ...)
    lines(xx, yy)
    invisible(cbind(x=xx,y=yy))
}

