lambda <- 1
m <- 2
f <- function(x){
  return(lambda^m * x^(m-1) * exp(-lambda * x) / gamma(m))
}

# step 2: Define a function h(x)=lambda/m*exp(-lambda/m*x)     ---  the exponential density at x
## [ADD CODE HERE]
h <- function(x){
  return(lambda / m * exp(- lambda / m * x))
} 

# step 3: Define the value of k:  k=m^m*exp(1-m)/gamma(m)    --- in general. for m=2, lambda=1 we have k=4/e
## [ADD CODE HERE]
k <- m^m * exp(1 - m) / gamma(m)

while (TRUE) {    # keep sampling x's from h(x) and testing them until you accept one
  X <- -log(runif(1))*m/lambda     # generate an x from h, the exponential density              
  
  # Now generate Y from Unif[0,Kh(x)] .....[ADD CODE HERE]
  Y <- runif(0, k * h(X))
  
  # and then
  if (Y < f(X)) return(X)   # this does the rejection step
}