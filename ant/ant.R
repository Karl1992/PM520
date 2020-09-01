location <- c(0, 20.2, runif(1,20.2,38.7), 38.7, 64.9, 81.8)
flag <- c('a','b','c','d','e','f')
speed <- c(1,1,-1,1,1,-1)
T <- 0
while(length(location)>1){
  T <- T+1
  for (i in 1:length(location)){
    location[i] <- location[i] + speed[i]
  }
  left <- which(speed < 0)
  if (length(left) > 1){
    if(left[1] > 1){
      for (j in 1:length(left)){
        if (location[left[j]] < location[left[j]-1]){
          tmp1 <- speed[left[j]]  
          speed[left[j]] <- speed[left[j]-1]
          speed[left[j]-1] <- tmp1
          tmp2 <- location[left[j]]  
          location[left[j]] <- location[left[j]-1]
          location[left[j]-1] <- tmp2
        }
      }
    }
  }
  k <- 1
  while (k <= length(location)){
    if ((speed[k] < 0 & location[k] < 0) | (speed[k] > 0 & location[k] > 100)){
      location <- location[-k]
      flag <- flag[-k]
      speed <- speed[-k]
    }
    k <- k+1
  }
}
T <- T + 100 - location
T
flag