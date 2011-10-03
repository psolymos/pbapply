library(dcmle)
jaw <- makeDcFit(
    data = list(
        "M" = 4,
        "N" = 20,
        "Y" =
            structure(c(47.8, 46.4, 46.3, 45.1, 47.6, 52.5, 51.2, 49.8, 48.1, 
            45, 51.2, 48.5, 52.1, 48.2, 49.6, 50.7, 47.2, 53.3, 46.2, 46.3, 
            48.8, 47.3, 46.8, 45.3, 48.5, 53.2, 53, 50, 50.8, 47, 51.4, 49.2, 
            52.8, 48.9, 50.4, 51.7, 47.7, 54.6, 47.5, 47.6, 49, 47.7, 47.8, 
            46.1, 48.9, 53.3, 54.3, 50.3, 52.3, 47.3, 51.6, 53, 53.7, 49.3, 
            51.2, 52.7, 48.4, 55.1, 48.1, 51.3, 49.7, 48.4, 48.5, 47.2, 49.3, 
            53.7, 54.5, 52.7, 54.4, 48.3, 51.9, 55.5, 55, 49.8, 51.8, 53.3, 
            49.5, 55.3, 48.4, 51.8), .Dim = as.integer(c(20, 4))),
        "age" =
            c(8, 8.5, 9, 9.5),
        "R" =
            structure(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), .Dim = c(4, 
            4))),
    model = function() {
      for (i in 1:N) {
         Y[i,] ~ dmnorm(mu[], Omega[,]);  # The 4 measurements for each  
      }                                   # boy are multivariate normal

      for(j in 1:M) {     # location model for mean bone length at each age
         mu[j] <- beta0 + beta1 * (age[j] - mean(age)); # linear
      }
      beta0.uncentred <- beta0 - beta1 * mean(age);

      beta0 ~ dnorm(0.0, 0.001); 
      beta1 ~ dnorm(0.0, 0.001); 
      Omega[1:M,1:M] ~ dwish(R[,], 4);	# between-child variance in length at each age	
      Sigma2[1:M,1:M] <- inverse(Omega[,]);

      for (i in 1:N) {
         for  (j in 1:M) {
            resid[i,j] <- Y[i,j] - mu[j];         # residuals
            resid2[i,j] <- resid[i,j]^2;     # squared residuals
         } 
      }
      RSS <- sum(resid2[,]);                    # Residual Sum of Squares
    },
    params = c("beta0.uncentred","beta1","Sigma2","mu","RSS"))
#dcmle(jaw)
