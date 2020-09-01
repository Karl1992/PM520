TotalRoute <- function(Route,Map_Point){
  D <- numeric()
  for (p in 1:(length(Route)-1)){
   D[p] <- dist(rbind(Map_Point[Route[p],],Map_Point[Route[p+1],])) 
  }
  D <- c(D,sum(D))
  return(D)
}

Routeplot <- function(Route,RDS,Map_Point){
  plot(Map_Point, pch = 12)
  points(Map_Point[Route[1],], col = "red", pch = 12)
  legend(Map_Point[Route[1],]-0.25, legend = "SP", bty = "n", cex = 0.75)
  for (p in 1:(length(Route)-1)){
  arrows(x0 = Map_Point[Route[p],1], y0 = Map_Point[Route[p],2], x1 = Map_Point[Route[p+1],1], y1 = Map_Point[Route[p+1],2], angle = 15, length = 0.1)  
  }
  legend("topleft", legend = c("total", round(RDS[length(RDS)],2)), bty = "n", cex = 1)
}



TSP <- function(InitialTemp, FinalTemp, Tempderate){
  
  MP <- read.table(choose.files())
  
  colnames(MP) <- c("x","y")
  
  R <- sample(nrow(MP),nrow(MP))
  
  R <- c(R,R[1])
  
  Temp <- InitialTemp
  
  RS <- TotalRoute(R,MP)
  
  Routeplot(R,RS,MP)
  
  while(Temp > FinalTemp){
    
    RS <- TotalRoute(R,MP)
    
    c <- sample(length(R)-1,2)
    
    NewR <- R
    
    NewR[c[1]:c[2]] <- NewR[c[2]:c[1]]
    
    NewR[length(NewR)] <- NewR[1]
    
    NewRS <- TotalRoute(NewR,MP)
    
    dE <- NewRS[length(NewRS)] - RS[length(RS)]
    
    h <- min(1, exp(-dE/Temp))
    
    p <- runif(1)
    
    if(p < h){
      R <- NewR
      
      Routeplot(R,RS,MP)
      
      Start.Time<-Sys.time()
      while (Sys.time() < Start.Time+0.1){}
    }
    
    Temp <- Temp * (1 - Tempderate)
  }
}

TSP(10,0.01,0.005)