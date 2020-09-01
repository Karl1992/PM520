# define the palette
mapcolor <- rainbow(4)
palette(mapcolor)

# install necessary package
  library("sp")
  library("maptools")
  library("plyr")
  # install.packages("spdep")
  library("spdep")

# set relative path
setwd(choose.dir())

# read map
chinamap <- readShapePoly("bou2_4p.shp")
proj4string(chinamap) <- "+proj=longlat +datum=WGS84"

# make a new map only have province data
chinamap <- chinamap[chinamap$AREA>0.1,]
chinamap <- chinamap[-which(chinamap$ADCODE99 == 130000)[2],]
chinamapdata <- chinamap@data

# get coordinates for provinces
map_crd <- coordinates(chinamap)

# construct the spatial weight matrix
w_cn <- poly2nb(chinamap,queen = T)
w_cn[[31]] <- c(which(chinamapdata$ADCODE99 == 440000))
w_cn[[30]] <- sort(c(which(chinamapdata$ADCODE99 == 440000), which(chinamapdata$ADCODE99 == 330000), which(chinamapdata$ADCODE99 == 350000)))
w_cn[[which(chinamapdata$ADCODE99 == 440000)]] <- as.integer(sort(c(w_cn[[which(chinamapdata$ADCODE99 == 440000)]], 30,31)))
w_cn[[which(chinamapdata$ADCODE99 == 330000)]] <- as.integer(sort(c(w_cn[[which(chinamapdata$ADCODE99 == 330000)]], 30)))
w_cn[[which(chinamapdata$ADCODE99 == 350000)]] <- as.integer(sort(c(w_cn[[which(chinamapdata$ADCODE99 == 350000)]], 30)))

w_cn_mat <- nb2listw(w_cn, style="W", zero.policy=TRUE)

plot(chinamap)
points(map_crd[,c(1,2)], col = "red", pch = "*")
plot(w_cn_mat, coords = map_crd, pch = 19, cex = 0.1, col = "blue", add = T) # proved w_cn's relationship is right

# Start find the best plan by stimulated annealing
Startcolor <- numeric()
for(i in 1:nrow(map_crd)){
  Startcolor[i] <- sample(1:4,1)
}
InitialTemp <- 10
FinalTemp <- 0.01
TempDecreaseRate <- 0.002

# Evaluating method

Counting <- function(colormethod){
  R <- numeric()
  for(place in 1:length(w_cn)){
    listofneighbour <- w_cn[[place]]
    mistake <- 0
    for(i in listofneighbour){
      if(colormethod[place] == colormethod[i]){
        mistake <- mistake + 1
      }
    }
    R[place] <- mistake
  }
  return(c(R, sum(R)))
}

# Start stimualted annealing
Temp <- InitialTemp

chinamapcolor <- Startcolor
R <- Counting(chinamapcolor)

plot(chinamap, col = chinamapcolor)
for(n in 1:(nrow(map_crd)-1)){
  points(map_crd[[n,1]],map_crd[[n,2]] , col = "black", pch = as.character(R[n]), lwd = 2)
}
points(map_crd[[nrow(map_crd),1]]+1, map_crd[[nrow(map_crd),2]]-1, col = "black", pch = as.character(R[nrow(map_crd)]), lwd = 2)
legend("topleft", legend = c("total", R[length(R)]), bty = "n")



while (Temp > FinalTemp){  # we keep going until things cool down

  newchinamapcolor <- numeric()
  change <- max(R[-length(R)])
  for(i in 1:nrow(map_crd)){
    if(R[i] == change){
      newchinamapcolor[i] <- sample(1:4,1)
    }else{
      newchinamapcolor[i] <- chinamapcolor[i]
    }
  }
  
  NewR <- Counting(newchinamapcolor)
  
  h <- min(1, exp(-(NewR - R)/Temp))
  
  p <- runif(1)  # the random number that is going to help us decide whether to move
  
  if (p < h){
    chinamapcolor <- newchinamapcolor
    
    plot(chinamap, col = chinamapcolor)
    for(n in 1:(nrow(map_crd)-1)){
      points(map_crd[[n,1]],map_crd[[n,2]] , col = "black", pch = as.character(R[n]), lwd = 2)
    }
    points(map_crd[[nrow(map_crd),1]]+1, map_crd[[nrow(map_crd),2]]-1, col = "black", pch = as.character(R[nrow(map_crd)]), lwd = 2)
    legend("topleft", legend = c("total", R[length(R)]), bty = "n")
    

    Start.Time<-Sys.time()
    while (Sys.time() < Start.Time+0.1){}   # there is probably a better way of doing this
    
    # update the record of the function value:
    R <- NewR
  }
  #reduce temperature
  Temp<-Temp*(1-TempDecreaseRate)
  if(R[length(R)] == 0){
    break
  }
}

plot(chinamap, col = chinamapcolor)
for(n in 1:(nrow(map_crd)-1)){
  points(map_crd[[n,1]],map_crd[[n,2]] , col = "black", pch = as.character(R[n]), lwd = 2)
}
points(map_crd[[nrow(map_crd),1]]+1, map_crd[[nrow(map_crd),2]]-1, col = "black", pch = as.character(R[nrow(map_crd)]), lwd = 2)
legend("topleft", legend = c("total", R[length(R)]), bty = "n")

