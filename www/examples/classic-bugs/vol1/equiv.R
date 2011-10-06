## equiv: bioequivalence in a cross-over trial (BUGS Examples Vol. 1)
library(dcmle)
equiv <- makeDcFit(
    multiply = "N",
    data = list("Y" =
            structure(c(1.4, 1.64, 1.44, 1.36, 1.65, 1.08, 1.09, 1.25, 1.25, 
            1.3, 1.65, 1.57, 1.58, 1.68, 1.69, 1.31, 1.43, 1.44, 1.39, 1.52
            ), .Dim = c(10, 2)),
        "group" =
            c(1, 1, -1, -1, -1, 1, 1, 1, -1, -1),
        "N" = 10),
#        "T" = 2),
    model = function() {
       # Original model 
       for (i in 1:N) { 
          d[i] ~ dnorm(0,tau[2]);  # Subject random effect
          for (k in 1:2){
             Treat[i,k] <- group[i]*(k-1.5) + 1.5; # treatment given 
             Y[i,k] ~ dnorm(m[i,k], tau[1]);
             m[i,k] <- mu + pow(-1, Treat[i,k]-1)* phi /2   
                          + pow(-1, k-1)* pi /2 + d[i];
          }
       }
       tau[1] ~ dgamma(0.001, 0.001);
       tau[2] ~ dgamma(0.001, 0.001);
       sigma[1] <- sqrt(1/tau[1]);
       sigma[2] <- sqrt(1/tau[2]);
       pi ~ dnorm(0, 1.0E-06);
       phi ~ dnorm(0, 1.0E-06);
       mu ~ dnorm(0, 1.0E-06); 
       theta <- exp(phi);
       # Indicate whether 0.8 < theta < 1.2
       equivalence <- step(theta - 0.8) - step(theta - 1.2);
    },
    params = c("theta","equivalence","sigma"))
#dcmle(equiv, n.clones=1:2) # problem with chisq.diag and r.hat calculations
