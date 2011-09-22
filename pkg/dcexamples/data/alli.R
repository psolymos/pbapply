library(dcmle)
alli <- makeDcFit(
    data = list(
        "I" = 4,
        "J" = 2,
        "K" = 5,
        "n" =
            structure(c(39, 20, 24, 41, 16, 28, 29, 22), .Dim = as.integer(c(4, 
            2))),
        "X" =
            structure(c(23, 5, 5, 16, 7, 13, 8, 17, 4, 11, 11, 19, 0, 8, 
            7, 1, 2, 1, 2, 1, 1, 6, 6, 0, 2, 0, 1, 2, 3, 1, 3, 1, 8, 3, 5, 
            3, 5, 0, 5, 3), .Dim = as.integer(c(4, 2, 5)))),
    model = function() {
    # PRIORS
       alpha[1] <- 0;       # zero contrast for baseline food
       for (k in 2:K){
          alpha[k] ~ dnorm(0,0.00001);  # vague priors
       }
       # Loop around lakes:
       for (k in 1:K){
          beta[1,k] <- 0;   # corner-point contrast with first lake 
       } 
       for (i in 2:I) {     
          beta[i,1] <- 0;   # zero contrast for baseline food
          for (k in 2:K) {
             beta[i,k] ~ dnorm(0,0.00001); # vague priors
          }
       }
       # Loop around sizes:
       for (k in 1:K){
          gamma[1,k] <- 0;  # corner-point contrast with first size 
       }
       for (j in 2:J) {     
          gamma[j,1] <- 0;  # zero contrast for baseline food
          for ( k in 2:K){
             gamma[j,k] ~ dnorm(0,0.00001); # vague priors
          }
       }
    # LIKELIHOOD
       for (i in 1:I) {        # loop around lakes
          for (j in 1:J) {     # loop around sizes

       # Multinomial response
             X[i,j,] ~ dmulti( p[i,j,] , n[i,j]  );
             for (k in 1:K) {  # loop around foods
                p[i,j,k]        <- phi[i,j,k] / sum(phi[i,j,]);
                log(phi[i,j,k]) <- alpha[k] + beta[i,k]  + gamma[j,k];
             }
          }  
       }
    # TRANSFORM OUTPUT TO ENABLE COMPARISON WITH AGRESTI'S RESULTS
       for (k in 1:K) {        # loop around foods
          for (i in 1:I) {     # loop around lakes
             b[i,k] <- beta[i,k] - mean(beta[,k]);   # sum to zero constraint
          }
          for (j in 1:J) {     # loop around sizes
             g[j,k] <- gamma[j,k] - mean(gamma[,k]); # sum to zero constraint
          }
       }
    # FITTED VALUES
       for (i in 1:I) {     # loop around lakes
          for (j in 1:J) {     # loop around sizes
             for (k in 1:K) {     # loop around foods
                E[i,j,k] <- p[i,j,k] * n[i,j];
                OlogOE[i,j,k] <- X[i,j,k] * log( X[i,j,k] / E[i,j,k] );
             }
          }  
       }
       G2 <- 2 * sum( OlogOE[,,] );
    },
    params = c("b[1,1]","b[1,2]","b[1,3]","b[1,4]","b[1,5]"))
#dcmle(alli)

