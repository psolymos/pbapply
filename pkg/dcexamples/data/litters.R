library(dcmle)
litters <- makeDcFit(
    data = list(
        "G" = 2,
        "n" =
            structure(c(13, 12, 12, 11, 9, 10, 9, 9, 8, 11, 8, 10, 13, 10, 
            12, 9, 10, 9, 10, 5, 9, 9, 13, 7, 5, 10, 7, 6, 10, 10, 10, 7), .Dim = c(2, 
            16)),
        "N" = 16,
        "r" =
            structure(c(13, 12, 12, 11, 9, 10, 9, 9, 8, 10, 8, 9, 12, 9, 
            11, 8, 9, 8, 9, 4, 8, 7, 11, 4, 4, 5, 5, 3, 7, 3, 7, 0), .Dim = c(2, 
            16))),
    model = function() {
      for (i in 1:G)
      {
         for (j in 1:N)
         {
            r[i,j] ~ dbin(p[i,j], n[i,j]);
            p[i,j] ~ dbeta(a[i], b[i]) %_% T(,0.9999); 
         }

         a[i] <- nbar[i] * mu[i] * phi[i];
         b[i] <- nbar[i] * (1 - mu[i]) * phi[i];

         mu[i] ~ dunif(0, 1);
         phi[i] <- nbar[i] * theta[i] / (1 - theta[i]);

         theta[i] ~ dunif(0, 1);
         nbar[i] <- mean(n[i,]);
      }
    },
    params = c("mu","theta"))
#dcmle(litters)
