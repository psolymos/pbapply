SAVE <- FALSE
LONG <- FALSE
if (LONG) {
    n.adapt <- 1000
    n.update <- 5000
    n.iter <- 5000
    n.chains <- 3
    thin <- 1
} else {
    n.adapt <- 100
    n.update <- 100
    n.iter <- 100
    n.chains <- 2
    thin <- 1
}

