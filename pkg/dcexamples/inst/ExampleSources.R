## rats example from WinBUGS Manual
rats0 <- list(
    data = list(N = 30, T = 5, 
        Y = structure(c(151, 145, 147, 155, 135, 159, 141, 159, 177, 134, 
        160, 143, 154, 171, 163, 160, 142, 156, 157, 152, 154, 139, 146, 
        157, 132, 160, 169, 157, 137, 153, 199, 199, 214, 200, 188, 210, 
        189, 201, 236, 182, 208, 188, 200, 221, 216, 207, 187, 203, 212, 
        203, 205, 190, 191, 211, 185, 207, 216, 205, 180, 200, 246, 249, 
        263, 237, 230, 252, 231, 248, 285, 220, 261, 220, 244, 270, 242, 
        248, 234, 243, 259, 246, 253, 225, 229, 250, 237, 257, 261, 248, 
        219, 244, 283, 293, 312, 272, 280, 298, 275, 297, 350, 260, 313, 
        273, 289, 326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285, 
        286, 303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305, 
        338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 321, 
        334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30, 5)),
        x = c(8.0, 15.0, 22.0, 29.0, 36.0)),
    inits = list(alpha = c(250, 250, 250, 250, 250, 250, 250, 250, 250,
        250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
        250, 250, 250, 250, 250, 250, 250, 250),
        beta = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
        alpha.c = 150, beta.c = 10, tau.c = 1, tau.alpha = 1, tau.beta = 1),
    model = function() {
        for (i in 1:N) {
           for (j in 1:T) {
              mu[i,j] <- alpha[i] + beta[i]*(x[j] - x.bar)
              Y[i,j]   ~ dnorm(mu[i,j],tau.c)
           }
           alpha[i] ~ dnorm(alpha.c,tau.alpha)
           beta[i]  ~ dnorm(beta.c,tau.beta)
        }
        alpha.c   ~ dnorm(0,1.0E-4)
        beta.c    ~ dnorm(0,1.0E-4)
        tau.c     ~ dgamma(1.0E-3,1.0E-3)
        tau.alpha ~ dgamma(1.0E-3,1.0E-3)
        tau.beta  ~ dgamma(1.0E-3,1.0E-3)
        sigma.c     <- 1.0/sqrt(tau.c)
        sigma.alpha <- 1.0/sqrt(tau.alpha)
        sigma.beta  <- 1.0/sqrt(tau.beta)
        x.bar    <- mean(x[])
        alpha0   <- alpha.c - beta.c*x.bar
    })
rownames(rats0$data$Y) <- paste("rat", 1:30, sep=".")
colnames(rats0$data$Y) <- paste("week", 1:5, sep=".")
names(rats0$data$x) <- paste("week", 1:5, sep=".")
rats <- new("dcFit")
rats@data <- rats0$data
rats@model <- custommodel(rats0$model)
rats@params <- c("alpha0", "beta.c", "sigma.c", "sigma.alpha", "sigma.beta")
rats@multiply <- "N"
rats@unchanged <- c("T", "x")
#rats@inits <- rats0$inits
#rats@initsfun <- function(model, n.clones) {
#    dclone(rats@inits, n.clones,
#        unchanged=c("alpha0", "beta.c", "tau.c", "tau.alpha", "tau.beta"))
#}
#dcmle(rats,n.clones=1:2,n.iter=1000)

## Seeds example from BUGS Vol. I.
seeds0 <- list(
    data = list(N = 21,
        r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 
            3, 22, 15, 32, 3),
        n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 
            4, 12, 41, 30, 51, 7),
        x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)),
    inits = list(tau = 1, alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha12 = 0),
    model = function() {
        alpha0  ~ dnorm(0.0,1.0E-6);     # intercept
        alpha1  ~ dnorm(0.0,1.0E-6);     # seed coeff
        alpha2  ~ dnorm(0.0,1.0E-6);     # extract coeff
        alpha12 ~ dnorm(0.0,1.0E-6);     # intercept
        tau     ~ dgamma(1.0E-3,1.0E-3); # 1/sigma^2
        sigma  <- 1.0/sqrt(tau);
        for (i in 1:N) {
           b[i]         ~ dnorm(0.0,tau);
           logit(p[i]) <- alpha0 + alpha1*x1[i] + alpha2*x2[i] +
                          alpha12*x1[i]*x2[i] + b[i];
           r[i]         ~ dbin(p[i],n[i]);
        }
    })
seeds <- new("dcFit")
seeds@data <- seeds0$data
seeds@inits <- seeds0$inits
seeds@model <- custommodel(seeds0$model)
seeds@multiply <- "N"
seeds@params <- c("alpha0", "alpha1", "alpha2", "alpha12", "sigma")

#dcmle(seeds,n.clones=1:2,n.iter=1000)

## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
paramecium <- new("dcFit")
paramecium@data <- list(ncl=1, n=length(paurelia), Y=dcdim(data.matrix(paurelia)))
paramecium@model <- custommodel(beverton.holt <- function() {
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
})
paramecium@multiply <- "ncl"
paramecium@unchanged <- "n"
paramecium@params <- c("lambda","beta","sigma")
#dcmle(paramecium,n.clones=1:2,n.iter=1000)
