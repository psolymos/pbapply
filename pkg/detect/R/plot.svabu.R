plot.svabu <-
function(x, type = 1:2, show = TRUE, ...) {
    ## eval of type
    if (!all(type %in% 1:2))
        stop("'type' must be in 1:2")
    n <- x$nobs
    c01 <- table(x$y)
    phi <- if(x$zeroinfl)
        x$zif.probabilities else rep(0, n)
    lambda <- fitted(x)
    ## qq
    delta <- x$detection.probabilities
    counts <- unique(x$y)
    counts <- counts[order(counts)]
    ## empirical quantiles
    cdfe <- cumsum(c01/n)
    ## theoretical quantiles
    dft.p <- rep(NA, length(counts))
    dft.zip <- rep(NA, length(counts))
    dft.p[1] <- mean(exp(-lambda * delta))
    dft.zip[1] <- mean(phi + (1 - phi) * exp(-lambda * delta))
    N <- 0:x$N.max
    N.rep <- rep(N, length(N))
    lambda.rep <- rep(lambda, each=length(N))
    delta.rep <- rep(delta, each=length(N))
    dp <- dpois(N.rep, lambda=lambda.rep)
    for (i in 2:length(counts)) {
        y.rep <- rep(counts[i], each=n * length(N))
        db <- dbinom(y.rep, N.rep, delta.rep)
        dft.p[i] <- mean(colSums(matrix(dp * db, nrow=length(N))))
        dft.zip[i] <- mean((1 - phi) * colSums(matrix(dp * db, nrow=length(N))))
    }
    fit <- if (x$zeroinfl)
        dft.zip else dft.p
    cdf <- data.frame(counts, empirical=cdfe, fitted=cumsum(fit))
    his <- data.frame(counts, empirical=as.integer(c01), fitted=fit*sum(c01))
    ## hist
    p0 <- mean((1 - phi) * exp(-lambda))
    pp <- data.frame(counts, fitted=fit)
    probs <- list(zif=mean(phi), pois0=p0, det0=pp$fitted[1] - mean(phi) - p0, counts=pp)
    if (show) {
        if (length(type) > 1)
            opar <- par(mfrow=c(1,2))
        if (1 %in% type) {
            ## observed counts
            d <- (1 - 0.2)/2
            plot(c01, xlim=c(-1-d, max(x$y)+d), ylab="Frequency", type="n", ...)
            for(i in 1:length(counts))
                polygon(rep(c(counts[i]-d, counts[i]+d), each=2),
                    c(0, c01[i], c01[i], 0), col="grey")
            ## theoretical values
            lines(counts[-1], fit[-1]*sum(c01[-1]), lty=2)
            points(counts[-1], fit[-1]*sum(c01[-1]), pch=19, col="white")
            points(counts[-1], fit[-1]*sum(c01[-1]), pch=21, col=1)
            ## Poisson zeros
            polygon(rep(c(-1-d, -1+d), each=2),
                c(0, p0 * n, p0 * n, 0), col="white")
            ## zero-inflation zeros
            if (x$zeroinfl) {
                polygon(rep(c(-1-d, -1+d), each=2),
                    c(p0 * n, (mean(phi) + p0) * n, (mean(phi) + p0) * n, p0 * n), col="black")
                legend("topright", legend=c("Observed counts", "Fitted Poisson", "Zero inflation"),
                    fill=c("grey", "white", "black"), pch=c(NA,21,NA), lty=c(NA,2,NA))
            } else {
                legend("topright", legend=c("Observed counts", "Fitted Poisson"), 
                    fill=c("grey", "white"), pch=c(NA,21), lty=c(NA,2))
            }
        }
        if (2 %in% type) {
            offset <- 0.02
            with(cdf, plot(fitted, empirical, xlim=range(cdf[,-1], cdf[,-1]+offset), type="b", 
                ylab="Empirical CDF", xlab="Fitted CDF", ...))
            with(cdf, text(fitted+offset, empirical, counts))
            abline(0, 1, lty=2)
        }
        if (length(type) > 1)
            par(opar)
    }
    invisible(list(probs=probs, hist=his, cdf=cdf))
}

