## pump: conjugate gamma-Poisson hierarchical model (BUGS Examples Vol. 1)
library(dcmle)
pump <- makeDcFit(
    multiply = "N",
    data = list(
        "N" = 10,
        "t" =
            c(94.3, 15.7, 62.9, 126, 5.24, 31.4, 1.05, 1.05, 2.1, 10.5),
        "x" =
            c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)),
    model = function() {
      for (i in 1:N){
          theta[i] ~ dgamma(alpha,beta);
          lambda[i] <- theta[i]*t[i];
          x[i] ~ dpois(lambda[i])
      }
      alpha ~ dexp(1.0);
      beta ~ dgamma(0.1,1.0);
    },
    params = c("theta","alpha","beta"))
#dcmle(pump,n.clones=1:2)
