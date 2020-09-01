n<-100000
count<-0
for (i in 1:n){
  point<-runif(2,-1,1)
  if (point[1]**2+point[2]**2 <= 1){
    count<-count+1
  }
}
spi<-count*4/n
cat("\nOur estimate of the expected pi is ",spi)