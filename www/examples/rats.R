## Rats example from BUGS Vol. I.
rats <- list(
    data = list(N = 30, T = 5, 
        Y = structure(c(151, 145, 147, 155, 135, 159, 141, 159, 177, 134, 
        160, 143, 154, 171, 163, 160, 142, 156, 157, 152, 154, 139, 146, 
        157, 132, 160, 169, 157, 137, 153, 199, 199, 214, 200, 188, 210, 
        189, 201, 236, 182, 208, 188, 200, 221, 216, 207, 187, 203, 212, 
        203, 205, 190, 191, 211, 185, 207, 216, 205, 180, 200, 246, 249, 
        263, 237, 230, 252, 231, 248, 285, 220, 261, 220, 244, 270, 242, 
        248, 234, 243, 259, 246, 253, 225, 229, 250, 237, 257, 261, 248, 
        219, 244, 283, 293, 312, 272, 280, 298, 275, 297, 350, 260, 313, 
        273, 289, 326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285, 
        286, 303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305, 
        338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 321, 
        334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30, 5)),
        x = c(8.0, 15.0, 22.0, 29.0, 36.0)),
    inits = list(alpha = c(250, 250, 250, 250, 250, 250, 250, 250, 250,
        250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
        250, 250, 250, 250, 250, 250, 250, 250),
        beta = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
        alpha.c = 150, beta.c = 10, tau.c = 1, tau.alpha = 1, tau.beta = 1),
    model = function() {
        for (i in 1:N) {
           for (j in 1:T) {
              mu[i,j] <- alpha[i] + beta[i]*(x[j] - x.bar)
              Y[i,j]   ~ dnorm(mu[i,j],tau.c)
           }
           alpha[i] ~ dnorm(alpha.c,tau.alpha)
           beta[i]  ~ dnorm(beta.c,tau.beta)
        }
        alpha.c   ~ dnorm(0,1.0E-4)
        beta.c    ~ dnorm(0,1.0E-4)
        tau.c     ~ dgamma(1.0E-3,1.0E-3)
        tau.alpha ~ dgamma(1.0E-3,1.0E-3)
        tau.beta  ~ dgamma(1.0E-3,1.0E-3)
        sigma.c     <- 1.0/sqrt(tau.c)
        sigma.alpha <- 1.0/sqrt(tau.alpha)
        sigma.beta  <- 1.0/sqrt(tau.beta)
        x.bar    <- mean(x[])
        alpha0   <- alpha.c - beta.c*x.bar
    })
rownames(rats$data$Y) <- paste("rat", 1:30, sep=".")
colnames(rats$data$Y) <- paste("week", 1:5, sep=".")
names(rats$data$x) <- paste("week", 1:5, sep=".")
#m <- jags.fit(rats$data, c("alpha0", "beta.c"), rats$model, rats$inits)
#mm <- dc.fit(rats$data, c("alpha0", "beta.c"), rats$model, NULL,
#    n.clones = 1:5, multiply = "N", unchanged = c("T", "x"))
