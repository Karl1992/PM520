library(adaptMCMC)
library(coda)
library(mvtnorm)

mu.vector <- c(0,0)
variance.matrix <- cbind(c(1,0.9), c(0.9,1))
our.data <- rmvnorm(t, mu.vector, variance.matrix)

NumberOfIterations <- 100000

adapt.log <- function(x, mu, sigMA){
  return(sum(log(dmvnorm(x, mean = mu, sigma = sigMA))))
}

new.variance.matrix <- cbind(c(1, sqrt(1 - 0.9^2)), c(sqrt(1 - 0.9^2), 1))

adapt_1 <- MCMC(adapt.log, NumberOfIterations, init = our.data[100,], scale = c(1, 0.1), adapt = T, acc.rate = 0.25, mu = mu.vector, sigMA = new.variance.matrix)

adapt_2 <- MCMC(adapt.log, NumberOfIterations, init = our.data[100,], scale = c(1, 0.1), adapt = T, acc.rate = 0.25, mu = mu.vector, sigMA = new.variance.matrix)

adapt_3 <- MCMC(adapt.log, NumberOfIterations, init = our.data[100,], scale = c(1, 0.1), adapt = T, acc.rate = 0.25, mu = mu.vector, sigMA = new.variance.matrix)

corr_adapt <- cor(adapt_1$samples[,1], adapt_1$samples[,2])

adapt_1_sample <- adapt_1$samples

adapt_2_sample <- adapt_2$samples

adapt_3_sample <- adapt_3$samples

adapt_1_sample <- mcmc(adapt_1_sample)
adapt_2_sample <- mcmc(adapt_2_sample)
adapt_3_sample <- mcmc(adapt_3_sample)

print(summary(adapt_1_sample))
autocorr.plot(adapt_1_sample,lag = 100, main = "Autocorrelation for adapt_1")

adapt_sample <- mcmc.list(list(adapt_1_sample, adapt_2_sample, adapt_3_sample))

cat("\nGelman test results follow...")
print(gelman.diag(adapt_sample))
gelman.plot(adapt_sample, main = "Gelman plots")

plot(adapt_1_sample, main = "Results for adapt_1")

plot(adapt_2_sample, main = "Results for adapt_2")

acf(adapt_2_sample)