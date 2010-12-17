## dcextras documentation checks for dontrun pieces
library(dcextras)

## ?bugs.parfit #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data(schools)
dat <- list(J = nrow(schools), y = schools$estimate, sigma.y = schools$sd)
bugs.model <- function(){
       for (j in 1:J){
         y[j] ~ dnorm (theta[j], tau.y[j])
         theta[j] ~ dnorm (mu.theta, tau.theta)
         tau.y[j] <- pow(sigma.y[j], -2)
       }
       mu.theta ~ dnorm (0.0, 1.0E-6)
       tau.theta <- pow(sigma.theta, -2)
       sigma.theta ~ dunif (0, 1000)
     }  
inits <- NULL
param <- c("mu.theta", "sigma.theta")
cl <- makeSOCKcluster(3)
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=1:3)
sim2 <- bugs.parfit(cl, dat, param, bugs.model, inits, program="openbugs",
    n.thin=1, seed=1:3)
## examples on how to use initial values
## self contained function
inits <- function() list(mu.theta=rnorm(1), sigma.theta=rlnorm(1))
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=1:3)
sim2 <- bugs.parfit(cl, dat, param, bugs.model, inits, program="openbugs",
    n.thin=1, seed=1:3)
## function pointing to the global environment
fun <- function() list(mu.theta=rnorm(1), sigma.theta=rlnorm(1))
inits <- function() fun()
clusterExport(cl, "fun")
sim <- bugs.parfit(cl, dat, param, bugs.model, inits, seed=1:3)
sim2 <- bugs.parfit(cl, dat, param, bugs.model, inits, program="openbugs",
    n.thin=1, seed=1:3)
stopCluster(cl)
