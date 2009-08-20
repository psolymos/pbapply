## tests linear relationship for binomial GLM
lincovtest <-
function(y, x, probs = seq(0, 1, 0.25), na.rm = FALSE)
{
    noNA <- which(apply(is.na(cbind(y,x)), 1, sum) == 0)
    if (!na.rm && length(noNA) != length(y))
        stop("NA values not allowed")
    if (na.rm) {
        y <- y[noNA]
        x <- x[noNA]
    }
    q <- qvector(x, probs = seq(0, 1, 0.25))
    z <- as.factor(q)
    y <- ifelse(y > 0, 1, 0)
    m <- coef(glm(y ~ z, family=binomial))
    names(m) <- levels(z)
    class(m) <- c("lincovtest", class(m))
    return(m)
}
