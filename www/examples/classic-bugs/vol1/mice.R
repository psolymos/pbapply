## mice: Weibull regression in censored survival analysis (BUGS Examples Vol. 1)
library(dcmle)
mice <- makeDcFit(
    data = list(
        "group" =
            c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 
            3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 
            4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
        "is.censored" =
            c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
            0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0),
        "last.t" =
            c(40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 
            40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 
            40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 10, 40, 40, 40, 40, 
            40, 40, 40, 40, 40, 24, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 
            40, 40, 40, 40, 20, 40, 40, 40, 40, 29, 10, 40, 40, 40, 40, 40, 
            40),
        "M" = 4,
        "N" = 80,
        "t" =
            c(12, 17, 21, 25, 11, 26, 27, 30, 13, 12, 21, 20, 23, 25, 23, 
            29, 35, NA, 31, 36, 32, 27, 23, 12, 18, NA, NA, 38, 29, 30, NA, 
            32, NA, NA, NA, NA, 25, 30, 37, 27, 22, 26, NA, 28, 19, 15, 12, 
            35, 35, 10, 22, 18, NA, 12, NA, NA, 31, 24, 37, 29, 27, 18, 22, 
            13, 18, 29, 28, NA, 16, 22, 26, 19, NA, NA, 17, 28, 26, 12, 17, 
            26)),
    model = function() {
      for(i in 1:N) {                          
            is.censored[i] ~ dinterval(t[i], last.t[i]);
            t[i] ~ dweib(r,mu[i]);  
                                               
            mu[i] <- exp(beta[group[i]]);      # relative risk model
      }
      for(j in 1:M) {
            beta[j] ~ dnorm(0.0, 0.0001);      # prior
            median[j] <- pow(log(2) *          # median survival
                           exp(-beta[j]), 1/r);  
      }
      r ~ dgamma(1.0,0.0001);      # slowly decreasing on +ve reals

      irr.control <- beta[1];                  # change 
      veh.control <- beta[2]-beta[1];          # parameterisation           
      test.sub <- beta[3]-beta[1];
      pos.control <- beta[4]-beta[1];
    },
    inits = list(
        "beta" = c(-13,-13,-13,-13),
        "r" = 4,
        "t" =
            c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
            NA, NA, 41, NA, NA, NA, NA, NA, NA, NA, 41, 41, NA, NA, NA, 41, 
            NA, 41, 41, 41, 41, NA, NA, NA, NA, NA, NA, 11, NA, NA, NA, NA, 
            NA, NA, NA, NA, NA, 25, NA, 41, 41, NA, NA, NA, NA, NA, NA, NA, 
            NA, NA, NA, NA, 21, NA, NA, NA, NA, 30, 11, NA, NA, NA, NA, NA, 
            NA)),
    params = c("veh.control","test.sub","pos.control","r","median"))
#dcmle(mice)
