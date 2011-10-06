## beetles: logistic, probit and extreme value models (BUGS Examples Vol. 2)
library(dcmle)
beetles <- makeDcFit(
    multiply = "N",
    data = list(
    "x" =
        c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.861, 1.8839),
    "n" =
        c(59, 60, 62, 56, 63, 59, 62, 60),
    "r" =
        c(6, 13, 18, 28, 52, 53, 61, 60),
    "N" = 8),
    model = function() {
       for (i in 1:N) {
          r[i] ~ dbin(p[i], n[i]);
          logit(p[i])   <- alpha.star + beta*(x[i]-mean(x[])); 
          # log likelihood for sample i & saturated log-likelihood:
          llike[i]     <- r[i]*log(p[i]) + (n[i]-r[i])*log(1-p[i]);  
          llike.sat[i] <- r[i]*log(r[i]/n[i]) + (n[i]-r[i])*log(1-r[i]/n[i]);
          r.hat[i] <- p[i]*n[i];  # fitted values
        }
        alpha.star ~ dnorm(0.0, 1.0E-3);
        beta      ~ dnorm(0.0, 1.0E-3);
        alpha     <- alpha.star - beta*mean(x[]);

        D <- 2 * (sum(llike.sat[]) - sum(llike[]));
    },
    params = c("alpha","beta","r.hat","D"))
#dcmle(beetles,n.clones=1:2)
