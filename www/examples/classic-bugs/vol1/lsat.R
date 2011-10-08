## lsat: latent variable models for item-response data (BUGS Examples Vol. 1)
library(dcmle)
lsat <- makeDcFit(
    data = list(
        "response" =
            structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 
            1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 
            0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 
            0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 
            1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
            1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), .Dim = c(32, 
            5)),
#        "m" =
#            c(3, 6, 2, 11, 1, 1, 3, 4, 1, 8, 0, 16, 0, 3, 2, 15, 10, 29, 
#            14, 81, 3, 28, 15, 80, 16, 56, 21, 173, 11, 61, 28, 298),
        "culm" =
            c(3, 9, 11, 22, 23, 24, 27, 31, 32, 40, 40, 56, 56, 59, 61, 76, 
            86, 115, 129, 210, 213, 241, 256, 336, 352, 408, 429, 602, 613, 
            674, 702, 1000),
        "N" = 1000,
        "R" = 32,
        "T" = 5),
    model = function() {
    # Rasch model
       for (j in 1:N) {
          for (k in 1:T) {
             logit(p[j,k]) <- beta*theta[j] - alpha[k];
             r[j,k] ~ dbern(p[j,k]);
          }
          theta[j] ~ dnorm(0,1);
       }
    # Priors
       for (k in 1:T) {
          alpha[k] ~ dnorm(0,0.0001);
          a[k] <- alpha[k] - mean(alpha[]);
       }
       beta ~ dnorm(0,0.0001) %_% T(0,);
    # Compute probability of response pattern i, for later use in computing G^2
      theta.new ~ dnorm(0,1);          # ability parameter for random student 
      for(k in 1:T) {
           logit(p.theta[k]) <- beta*theta.new - alpha[k];  
           for(i in 1:R) {
              p.item[i,k] <- p.theta[k]^response[i,k] * (1-p.theta[k])^(1-response[i,k]);
           }
      } 
      for(i in 1:R) {    
         # P_i|theta = PROD_k p_k|theta   
         P.theta[i] <- prod(p.item[i,]);
      }
    },
    params = c("alpha","beta"))

lsat@data$r <- matrix(0, lsat@data$N, lsat@data$T)
for (j in 1:lsat@data$culm[1]) {
   lsat@data$r[j,] <- lsat@data$response[1, ];
}
for (i in 2:lsat@data$R) {
   for (j in (lsat@data$culm[i-1] + 1):lsat@data$culm[i]) {
      lsat@data$r[j,] <- lsat@data$response[i, ];
   }
}
lsat@data$culm <- NULL

#dcmle(lsat)
