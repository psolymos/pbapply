simulateUsedAvail <- 
function(data, parms, n.used, m, link="logit") 
{
    X <- model.matrix(~., data)
    if (ncol(X) != length(parms))
        stop("length of 'parms' incompatible with 'data'")
    n.sites <- nrow(X)
    n.avail <- n.used * m
    linkinvfun <- binomial(link=make.link(link))$linkinv
    p <- drop(linkinvfun(X %*% parms))
    id1 <- sample.int(n.sites, n.used, replace = TRUE, prob = p)
    id2 <- sample.int(n.sites, n.avail, replace = TRUE)
    data.frame(status=c(rep(1, n.used), rep(0, n.avail)), 
        data[c(id1, id2),,drop=FALSE])
}
