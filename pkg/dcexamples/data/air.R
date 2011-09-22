library(dcmle)
air <- makeDcFit(
    data = list("alpha" = 4.48,
        "beta" = 0.76,
        "sigma2" = 81.14,
        "J" = 3,
        "y" = c(21, 20, 15),
        "n" = c(48, 34, 21),
        "Z" = c(10, 30, 50)),
    model = function() {
       theta[1] ~ dnorm(0.0,1.0E-3);  
       theta[2] ~ dnorm(0.0,1.0E-3);
       tau <- 1/sigma2;		
       for (j in 1:J) {
          mu[j]       <- alpha + beta*Z[j];
          X[j]         ~ dnorm(mu[j],tau);
          logit(p[j]) <- theta[1] + theta[2] * (X[j] - mean(X));
          y[j]         ~ dbin(p[j],n[j]);
       }
       theta0    <- theta[1] - theta[2]*mean(X);
    },
    params = c("theta0", "theta[2]", "X[1]", "X[2]", "X[3]"))
#dcmle(air)

