gamma.sim_hx <- function(lambda, m) {
  iter <- 0
  f <- function(x){
    return(lambda^m * x^(m-1) * exp(-lambda * x) / gamma(m))
  }
  
  h <- function(x){
    mu <- lambda / m
    return(mu * exp(-mu * x))
  }
  
  while (TRUE) {    
    X <- -log(runif(1))*m/lambda             
    Y <- runif(1, 0, h(X))
    iter <- iter + 1 
    if (Y < f(X)) return(c(X, iter))   # this does the rejection step
  }
}

set.seed(1999)

n <- 10000        # number of replicates
g.hx <- mat.or.vec(n, 2)      # create somewhere to keep the answers

for (i in 1:n) g.hx[i,] <- gamma.sim_hx(1, 2)    # generate your 10000 gamma r.v.s

hist(g.hx[,1], breaks=20, freq=F, xlab="x", ylab="pdf f(x)", main=c("theoretical and simulated gamma(1, 2) density", paste("iter =", sum(g.hx[,2])), paste("n =", n)))
x <- seq(0, max(g.hx), .1)
lines(x, dgamma(x, 2, 1))
