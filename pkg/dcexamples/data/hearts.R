library(dcmle)
hearts <- makeDcFit(
    data = list(
        "N" = 12,
        "y" =
            c(5, 2, 0, 0, 2, 1, 0, 0, 0, 0, 13, 0),
        "t" =
            c(11, 11, 17, 22, 9, 6, 5, 14, 9, 7, 22, 51)),
    model = function() {
       for (i in 1:N) {
          y[i] ~ dbin(P[state1[i]], t[i]);
          state[i] ~ dbern(theta);
          state1[i] <- state[i]+1;   # state[i] takes values 0 or 1, so need to
                                     # add 1 to get values for use as index on P
       }
       P[1] <- p;
       P[2] <- 0;
       logit(p) <- alpha; 
       alpha ~ dnorm(0,1.0E-4); 
       beta <- exp(alpha);  # beta measures change in rate of PVCs after treatment
       logit(theta) <- delta; 
       delta ~ dnorm(0,1.0E-4)
    },
    params = c("theta", "beta","p"))
#dcmle(hearts)
