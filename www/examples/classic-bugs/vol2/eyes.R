library(dcmle)
eyes <- makeDcFit(
    data = list(
        "N" = 48,
        "y" = c(529.0,530.0,532.0,533.1,533.4,533.6,533.7,534.1,534.8,535.3,
             535.4,535.9,536.1,536.3,536.4,536.6,537.0,537.4,537.5,538.3,
             538.5,538.6,539.4,539.6,540.4,540.8,542.0,542.8,543.0,543.5,
             543.8,543.9,545.3,546.2,548.8,548.7,548.9,549.0,549.4,549.9,
             550.6,551.2,551.4,551.5,551.6,552.8,552.9,553.2),
        "Itot" = c(1,1)),
    model = function() {
       for (i in 1:N){
           y[i]  ~ dnorm(lambda[T[i]],tau);       
           T[i]  ~ dcat(P[])
           for (j in 1:2) {
              ind[i,j] <- T[i] == j;
           }
       }
       for (j in 1:2) {
           tot[j] <- sum(ind[,j]);
           Itot[j] ~ dinterval(tot[j], 0);
       }
       sigma     <- 1/sqrt(tau);
       tau        ~ dgamma(0.01, 0.01);
       lambda[1]  ~ dnorm(0, 1.0E-6); 
       lambda[2] <- lambda[1] + theta;
       theta      ~ dnorm(0, 1.0E-6) %_% T(0,);
       P[1:2]        ~ ddirch(alpha[1:2]);  # prior for mixing proportion
       alpha[1]  <- 1;                  # uniform prior   
       alpha[2]  <- 1;
    },
    inits = list(
        "lambda" = c(535,NA),
        "theta" = 5,
        "tau" =  0.1,
        "T" = c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,
            1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)),
    params = c("P", "lambda","sigma"))
#dcmle(eyes)
