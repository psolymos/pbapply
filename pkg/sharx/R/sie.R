sie <- function(S, A) {
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
    res <- stats4:::mle(nll, inits)
    attr(res, "data") <- cbind(A, S)
    res@call <- match.call()
    res
}

coef.sie <- function(object, ...) {
    c(object$coefficients, That=object$That)
}
print.sie <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    if (length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(c(x$coefficients, That=x$That), digits = digits), print.gap = 2, 
            quote = FALSE)
    }
    else cat("No coefficients\n")
    cat("\n")
    invisible(x)
}
summary.sie <- function(object, ...) {
    n <- nobs(object)
    x <- stats:::summary.lm(object, ...)
    r <- x$r.squared
    x$adj.r.squared <- 1-(1-r*(n-1)/(n-4))
    x$df <- c(x$df[1]+1, x$df[2]-1, x$df[3])
    x$That <- object$That
    class(x) <- c("summary.sie", class(x))
    x
}
print.summary.sie <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    stats:::print.summary.lm(x, digits, ...)
    cat("SIE threshold =", round(x$That, digits), "\n\n")
    invisible(x)
    ## simplify stuff to hide incorrect df -- or use stats4:::mle ???
}
sieplot <- function(x, ...) {
    if (!inherits(x, "sie"))
        stop("'sie' class expected")
    logA <- log(x$data[,1])
    logS <- log(x$data[,2]+0.5)
    cfs <- coef(x)
    xx <- c(min(logA), cfs[3], max(logA))
    yy <- c(cfs[1], cfs[1], cfs[1]+cfs[2]*(max(logA)-cfs[3]))
    plot(logA, logS, xlab="log(A)", ylab="log(S+0.5)", ylim=range(logS, yy), ...)
    lines(xx, yy)
}
