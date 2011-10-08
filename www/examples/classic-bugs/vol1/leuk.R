## leuk: survival analysis using Cox regression (BUGS Examples Vol. 1)
## step is JAGS function, needs replacement
library(dcmle)
leuk <- makeDcFit(
    data = list(
        "N" =
            42,
        "T" =
            17,
        "eps" =
            1e-10,
        "obs.t" =
            c(1, 1, 2, 2, 3, 4, 4, 5, 5, 8, 8, 8, 8, 11, 11, 12, 12, 15, 
            17, 22, 23, 6, 6, 6, 6, 7, 9, 10, 10, 11, 13, 16, 17, 19, 20, 
            22, 23, 25, 32, 32, 34, 35),
        "fail" =
            c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 
            0),
        "Z" =
            c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, -0.5, -0.5, -0.5, 
            -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, 
            -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5),
        "t" =
            c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 15, 16, 17, 22, 23, 
            35)),
    model = function() {
       for(j in 1:T) {
          for(i in 1:N) {
             dN[i,j]   ~ dpois(Idt[i,j]);              # Likelihood
             Idt[i,j] <- Y[i,j] * exp(beta*Z[i]) * dL0[j]; # Intensity 
          }     
          dL0[j] ~ dgamma(mu[j], c);
          mu[j] <- dL0.star[j] * c;    # prior mean hazard
          dL0.star[j] <- r * (t[j+1]-t[j])  
          # Survivor function = exp(-Integral{l0(u)du})^exp(beta*z)    
          S.treat[j] <- pow(exp(-sum(dL0[1:j])), exp(beta * -0.5));
          S.placebo[j] <- pow(exp(-sum(dL0[1:j])), exp(beta * 0.5));	
       }
       beta ~ dnorm(0.0, 1.0E-6);                 
       c <- 0.001; 
       r <- 0.1; 
    },
    params = "beta")

leuk@data$dN <- leuk@data$Y <- matrix(0, leuk@data$N, leuk@data$T)
for(i in 1:leuk@data$N) {
   for(j in 1:leuk@data$T) {
      ## risk set = 1 if obs.t >= t
      leuk@data$Y[i,j] <- as.integer((leuk@data$obs.t[i] - 
          leuk@data$t[j] + leuk@data$eps) >= 0)
      ## counting process jump = 1 if obs.t in [ t[j], t[j+1] )
      ##                      i.e. if t[j] <= obs.t < t[j+1]
      leuk@data$dN[i,j] <- leuk@data$Y[i,j] * 
          leuk@data$fail[i] * as.integer((leuk@data$t[j+1] - 
          leuk@data$obs.t[i] - leuk@data$eps) >= 0)
   }
}
leuk@data$eps <- NULL
leuk@data$obs.t <- NULL
leuk@data$fail <- NULL
#dcmle(leuk)
