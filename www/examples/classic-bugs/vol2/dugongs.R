## dugongs: a nonconjugate, nonlinear model (BUGS Examples Vol. 2)
library(dcmle)
dugongs <- makeDcFit(
    multiply = "N",
    data = list(
        "N" = 27,
        "x" =
        c(1, 1.5, 1.5, 1.5, 2.5, 4, 5, 5, 7, 8, 8.5, 9, 9.5, 9.5, 10, 
        12, 12, 13, 13, 14.5, 15.5, 15.5, 16.5, 17, 22.5, 29, 31.5),
        "Y" =
        c(1.8, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 2.19, 
        2.26, 2.4, 2.39, 2.41, 2.5, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 
        2.47, 2.64, 2.56, 2.7, 2.72, 2.57)),
    model = function() {
       for (i in 1:N) {
          mu[i] <- alpha - beta * gamma^x[i]
          Y[i] ~ dnorm(mu[i], tau)
       }
       alpha ~ dnorm(0.0, 1.0E-6) 
       beta ~ dnorm(0.0, 1.0E-6) 
       gamma ~ dunif(0.5, 1)
       tau ~ dgamma(1.0E-3, 1.0E-3)
       sigma <- 1.0/sqrt(tau)
       U3 <- logit(gamma)
    },
    params = c("U3","alpha","beta","gamma","sigma"))
#dcmle(dugongs,n.clones=1:2)
