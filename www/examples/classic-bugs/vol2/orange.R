## orange: a hierarchical, nonlinear model (BUGS Examples Vol. 2)
library(dcmle)
orange <- makeDcFit(
    unchanged = "n",
    multiply = "K",
    data = list(
        "n" = 7,
        "K" = 5,
        "x" =
            c(118, 484, 664, 1004, 1231, 1372, 1582),
        "Y" =
            structure(c(30, 33, 30, 32, 30, 58, 69, 51, 62, 49, 87, 111, 
            75, 112, 81, 115, 156, 108, 167, 125, 120, 172, 115, 179, 142, 
            142, 203, 139, 209, 174, 145, 203, 140, 214, 177), .Dim = as.integer(c(5, 
            7)))),
    model = function() {
       for (i in 1:K) {
          for (j in 1:n) {
             Y[i, j] ~ dnorm(mean[i, j], tauC)
             mean[i, j] <- phi[i, 1] / (1 + phi[i, 2] * exp(phi[i, 3] * x[j]))
          }
          phi[i, 1] <- exp(theta[i, 1])
          phi[i, 2] <- exp(theta[i, 2]) - 1
          phi[i, 3] <- -exp(theta[i, 3])
          for (k in 1:3) {
             theta[i, k] ~ dnorm(mu[k], tau[k]) 
          }
       }
       tauC ~ dgamma(1.0E-3, 1.0E-3)
       sigmaC <- 1 / sqrt(tauC)
       for (k in 1:3) {
          mu[k] ~ dnorm(0, 1.0E-4)
          tau[k] ~ dgamma(1.0E-3, 1.0E-3)
          sigma[k] <- 1 / sqrt(tau[k])
       }
    },
    params = c("mu","sigma","sigmaC"))
#dcmle(orange,n.clones=1:2)
