original <- read.table("D:/study/MasterinUSC/pm520/week5/RegressionInput.txt", header = F, col.names = c("x", "y"))

SimAnneal2D<-function(dataset,StartPoint,InitialTemp,FinalTemp,TempDecreaseRate,StepSize){
  a <- StartPoint[1]
  b <- StartPoint[2]
  y <- a * dataset$x + b
  SS <- sum((y-original$y)^2)
  
  Temp <- InitialTemp
  
  plot(x = dataset$x, y = dataset$y, xlab = "x", ylab = "y", pch = 20, cex = 0.8, col = "black")
  abline(b, a, col = "green", lwd = 2) # set up a plot
  
  while (Temp > FinalTemp){  # we keep going until things cool down
    Newa <- a + rnorm(1,mean=0,sd=StepSize)  # you don't have to do it this way, but it seems reasonable to me
    Newb <- b + rnorm(1,mean=0,sd=StepSize)
    New <- c(Newa,Newb)
    #decide whether to move
    Newy <- Newa * dataset$x + Newb
    NewSS <- sum((Newy - original$y)^2)# the value the function takes at the new point
    
    h = min(1, exp(-(NewSS - SS)/Temp))
    
    p <- runif(1)  # the random number that is going to help us decide whether to move
    if (p < h){
      
      a <- Newa
      
      b <- Newb
      
      abline(b, a, col = "blue", lwd = 2)
      # pause for a bit - otherwise the plots flash by too quickly to see properly
      Start.Time<-Sys.time()
      while (Sys.time() < Start.Time+0.1){}   # there is probably a better way of doing this
      
      # update the record of the function value:
      SS <- NewSS
    }
    #reduce temperature
    Temp<-Temp*(1-TempDecreaseRate)
    
  }
  abline(b, a, col = "red", lwd = 5)   # a big blue point to show where we ended up
  return (cat("\n The regression line is", "y=", a, "*x+", b, "."))  # return the coordinates of our final resting place, and the function value
}

SimAnneal2D(original,c(0,0),10,0.1,0.02,0.5)

original_cubic <- read.table("D:/study/MasterinUSC/pm520/week5/CubicRegressionInput.txt", header = F, col.names = c("x", "y"))

SimAnneal4D<-function(dataset,StartPoint,InitialTemp,FinalTemp,TempDecreaseRate,StepSize,mu){
  a <- StartPoint[1]
  b <- StartPoint[2]
  c <- StartPoint[3]
  d <- StartPoint[4]
  y <- a * (dataset$x)^3 + b * (dataset$x)^2 + c * dataset$x + d
  SS <- sum((y-dataset$y)^2)
  
  Temp <- InitialTemp
  
  plot(x = dataset$x, y = dataset$y, xlab = "x", ylab = "y", pch = 20, cex = 0.8, col = "black")
  par(new = T)
  curve(a * x^3 + b * x^2 + c * x + d, col = "green", lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n") # set up a plot
  
  while (Temp > FinalTemp){  # we keep going until things cool down
    Newa <- a + rnorm(1,mean=mu,sd=StepSize)  # you don't have to do it this way, but it seems reasonable to me
    Newb <- b + rnorm(1,mean=mu,sd=StepSize)
    Newc <- c + rnorm(1,mean=mu,sd=StepSize)  
    Newd <- d + rnorm(1,mean=mu,sd=StepSize)
    New <- c(Newa,Newb,Newc,Newd)
    #decide whether to move
    Newy <- Newa * (dataset$x)^3 + Newb * (dataset$x)^2 + Newc * dataset$x + Newd
    
    NewSS <- sum((Newy - dataset$y)^2)
    
    h = min(1, exp(-(NewSS - SS)/Temp))
    
    p <- runif(1)  # the random number that is going to help us decide whether to move
    
    if (p < h){
      
      a <- Newa
      
      b <- Newb
      
      c <- Newc
      
      d <- Newd
      
      par(new = T)
      curve(a * x^3 + b * x^2 + c * x + d, col = "blue", lwd = 2, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      # pause for a bit - otherwise the plots flash by too quickly to see properly
      Start.Time<-Sys.time()
      while (Sys.time() < Start.Time+0.1){}   # there is probably a better way of doing this
      
      # update the record of the function value:
      SS <- NewSS
    }
    #reduce temperature
    Temp<-Temp*(1-TempDecreaseRate)
    
  }
  par(new = T)
  curve(a * x^3 + b * x^2 + c * x + d, col = "red", lwd = 4, xlab = "", ylab = "", xaxt = "n", yaxt = "n")   # a big blue point to show where we ended up
  return (c(a,b,c,d))  # return the coordinates of our final resting place, and the function value
}

SimAnneal4D(original_cubic,c(0,0,0,0),10,0.001,0.0001,0.5,0)


