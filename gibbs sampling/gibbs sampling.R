library(coda)
library(mcmc)
library(mvtnorm)
t <- 100
mu.vector <- c(0,0)
variance.matrix <- cbind(c(1,0.9), c(0.9,1))
our.data <- rmvnorm(t, mu.vector, variance.matrix)
gibbs <- function(rou, n, initial){
  iter <- 1
  result <- mat.or.vec(n,2)
  theta <- initial[t,1]
  mu <- initial[t,2]
  while (iter <= n){
    thetaprime <- rnorm(1, rou * mu, 1 - rou^2)
    muprime <- rnorm(1, rou * theta, 1 - rou^2)
    theta <- thetaprime
    mu <- muprime
    result <- rbind(result, c(theta, mu))
    iter <- iter + 1
  }
  return(result)
}

gibbs_r1 <- gibbs(0.9, 100000, our.data)
gibbs_r2 <- gibbs(0.9, 100000, our.data)
gibbs_r3 <- gibbs(0.9, 100000, our.data)

corr_gibbs <- cor(gibbs_r1[,1], gibbs_r1[,2])

gibbs_r1 <- mcmc(gibbs_r1)
print(summary(gibbs_r1))
autocorr.plot(gibbs_r1, lag = 100, main = "Autocorrelation for gibbs_r1")

gibbs_r2 <- mcmc(gibbs_r2)
gibbs_r3 <- mcmc(gibbs_r3)

gibbs_r <- mcmc.list(list(gibbs_r1, gibbs_r2, gibbs_r3))

cat("\nGelman test results follow...")
print(gelman.diag(gibbs_r))
gelman.plot(gibbs_r, main = "Gelman plots")

plot(gibbs_r1, main = "Results for gibbs_r1")
plot(gibbs_r2, main = "Results for gibbs_r2")
acf(gibbs_r2)

cat("\n Test data means:", mean(our.data[,1]), mean(our.data[,2]))
