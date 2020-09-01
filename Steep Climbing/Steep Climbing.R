fx <- function(a,b){
  return((1-a)^2 + 100*(b-a^2)^2)
}

x <- 0
y <- 0
startpoint <- c(x,y)
fxy <- fx(startpoint[1],startpoint[2])

for(i in 1:100000){
  if(fx(x,y) >= fxy){
    x <- x + rnorm(1,0,0.5)
    y <- y + rnorm(1,0,0.5)
  }else{
    fxy <- fx(x, y)
    final <- c(x, y)
  }
}
