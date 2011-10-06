## blocker: random effects meta-analysis of clinical trials (BUGS Examples Vol. 1)
library(dcmle)
blocker <- makeDcFit(
    multiply = "Num",
    data = list("rt" =
            c(3, 7, 5, 102, 28, 4, 98, 60, 25, 138, 64, 45, 9, 57, 25, 33, 
            28, 8, 6, 32, 27, 22),
        "nt" =
            c(38, 114, 69, 1533, 355, 59, 945, 632, 278, 1916, 873, 263, 
            291, 858, 154, 207, 251, 151, 174, 209, 391, 680),
        "rc" =
            c(3, 14, 11, 127, 27, 6, 152, 48, 37, 188, 52, 47, 16, 45, 31, 
            38, 12, 6, 3, 40, 43, 39),
        "nc" =
            c(39, 116, 93, 1520, 365, 52, 939, 471, 282, 1921, 583, 266, 
            293, 883, 147, 213, 122, 154, 134, 218, 364, 674),
        "Num" =
            22),
    model = function() {
       for (i in 1:Num) {
          rt[i] ~ dbin(pt[i], nt[i]);
          rc[i] ~ dbin(pc[i], nc[i]);
          logit(pc[i]) <- mu[i] 
          logit(pt[i]) <- mu[i] + delta[i];
          delta[i] ~ dnorm(d, tau);
          mu[i] ~ dnorm(0.0, 1.0E-5);
       }
       d ~ dnorm(0.0, 1.0E-6);
       tau ~ dgamma(1.0E-3, 1.0E-3);
       delta.new ~ dnorm(d,tau);
       sigma <- 1/sqrt(tau);
    },
    params = c("d","delta.new","sigma"))
#dcmle(blocker, n.clones=1:2, n.iter=1000)
