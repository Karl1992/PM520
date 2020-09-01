set.seed(2018)
n <- 1000
L <- 5
V_of_W <- rep(0,n)
for (i in 1:n) {
  RF <- rep(-1,365)
  for (j in 1:365){
    while (RF[j] < 0) {
      RF[j] <- rnorm(1,mean = 5, sd = 10) 
    }
    V_of_W[i] <- V_of_W[i] + RF[j]
  }
}

V_of_W <- sort(V_of_W)
V_N <- ceiling((1-0.2) * n)
V <- V_of_W[V_N]
hist(V_of_W)
abline(v = V, col = 'red', lwd = '2')
cat("\nThe dam's volume should be", V, "to make sure the probability that the dam will overflow at some point within
the next year is less than 20%.")
