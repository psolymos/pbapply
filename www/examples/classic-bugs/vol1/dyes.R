## dyes: variance components model (BUGS Examples Vol. 1)
## transpose is used for for DC, indexing flipped
library(dcmle)
dyes <- makeDcFit(
    unchanged = "BATCHES",
    multiply = "SAMPLES",
    data = list("y" =
#            structure(c(1545, 1540, 1595, 1445, 1595, 1520, 1440, 1555, 1550, 
#            1440, 1630, 1455, 1440, 1490, 1605, 1595, 1515, 1450, 1520, 1560, 
#            1510, 1465, 1635, 1480, 1580, 1495, 1560, 1545, 1625, 1445), .Dim = c(6, 
#            5)),
            structure(c(1545, 1440, 1440, 1520, 1580, 1540, 1555, 1490, 1560, 
            1495, 1595, 1550, 1605, 1510, 1560, 1445, 1440, 1595, 1465, 1545, 
            1595, 1630, 1515, 1635, 1625, 1520, 1455, 1450, 1480, 1445), .Dim = 5:6),
        "BATCHES" =
            6,
        "SAMPLES" =
            5),
    model = function() {
       for (i in 1:BATCHES) {
          for (j in 1:SAMPLES) {
             y[j,i] ~ dnorm(mu[i], tau.within);
          }
          mu[i] ~ dnorm(theta, tau.between);
       }
       theta ~ dnorm(0.0, 1.0E-10);
       tau.within ~ dgamma(0.001, 0.001);
       sigma2.within <- 1/tau.within;
       tau.between ~ dgamma(0.001, 0.001);
       sigma2.between <- 1/tau.between;
       
       sigma2.total <- sigma2.within + sigma2.between;
       f.within <- sigma2.within/sigma2.total;     
       f.between <- sigma2.between/sigma2.total;     
    },
    params = c("theta","sigma2.within","sigma2.between","f.between"))
#dcmle(dyes, n.clones=1:2, n.iter=1000)
