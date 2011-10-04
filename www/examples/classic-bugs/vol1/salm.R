## salm: extra-Poisson variation in dose-response study (BUGS Examples Vol. 1)
library(dcmle)
load.module("glm")
salm <- makeDcFit(
    data = list(
        "doses" = 6,
        "plates" = 3,
        "y" =
            structure(c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 
            21, 33, 60, 41, 42), .Dim = as.integer(c(6, 3))),
        "x" =
            c(0, 10, 33, 100, 333, 1000)),
    model = function() {
       alpha.star ~ dnorm(0.0,1.0E-4);     # intercept
       beta ~ dnorm(0.0,1.0E-4);      # mutagenic effect
       gamma ~ dnorm(0.0,1.0E-10);    # toxic effect
       tau ~ dpar(0.5, 0.04);         # Pareto prior on precision
#       tau ~ dgamma(1.0E-3,1.0E-3);   # Gamma prior on precision
       sigma <- 1.0/sqrt(tau);
       for(i in 1:doses){
          for(j in 1:plates){
             log(mu[i,j])   <- alpha.star + beta*(logx[i]-mean(logx[])) 
                               + gamma*(x[i]-mean(x[])) + lambda[i,j];
             y[i,j]          ~ dpois(mu[i,j]);
             lambda[i,j]     ~ dnorm(0.0,tau);
         }
         logx[i]        <- log(x[i]+10);
       }
       alpha <- alpha.star - beta*mean(logx[]) - gamma*mean(x[]);
    },
    inits = list(
        "tau" = 0.1,
        "alpha.star" = 0,
        "beta" = 0,
        "gamma" = 0),
    params = c("alpha","beta","gamma","sigma"))
#dcmle(salm)
