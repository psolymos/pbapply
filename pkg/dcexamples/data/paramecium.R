## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
library(dcmle)
paramecium <- makeDcFit(
    data = list(ncl=1, 
        n=18, 
        Y=dcdim(data.matrix(c(17,29,39,63,185,258,267,
            392,510,570,650,560,575,650,550,480,520,500)))),
    model = function() {
        for (k in 1:ncl) {
            for(i in 2:(n+1)){
                Y[(i-1), k] ~ dpois(exp(X[i, k])) # observations
                X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2) # state
                mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k]))
            }
            X[1, k] ~ dnorm(mu0, 1 / sigma^2) # state at t0
        }
        beta ~ dlnorm(-1, 1) # Priors on model parameters
        sigma ~ dlnorm(0, 1)
        tmp ~ dlnorm(0, 1)
        lambda <- tmp + 1
        mu0 <- log(2)  + log(lambda) - log(1 + beta * 2)
    },
    multiply = "ncl",
    unchanged = "n",
    params = c("lambda","beta","sigma"))
#dcmle(paramecium,n.clones=1:2,n.iter=1000)
