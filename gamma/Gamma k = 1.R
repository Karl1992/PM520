gamma.sim_1 <- function(lambda, m) {
  iter <- 0
  f <- function(x){
    return(lambda^m * x^(m-1) * exp(-lambda * x) / gamma(m))
  }
  
  K <- 1
  
  while (TRUE) {    
    X <- runif(1, 0, 20)             
    Y <- runif(1, 0, K)
    iter <- iter + 1 
    if (Y < f(X)) return(c(X, iter))   # this does the rejection step
  }
}

set.seed(1999)

n <- 10000        # number of replicates
g.1 <- mat.or.vec(n, 2)      # create somewhere to keep the answers

for (i in 1:n) g.1[i,] <- gamma.sim_1(1, 2)    # generate your 10000 gamma r.v.s

hist(g.1[,1], breaks=30, freq=F, xlab="x", ylab="pdf f(x)", main=c("theoretical and simulated gamma(1, 2) density", paste("iter =", sum(g.1[,2]))))
x <- seq(0, max(g.1[,1]), .1)
lines(x, dgamma(x, 2, 1))


