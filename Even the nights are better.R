Fomula <- function(x){
  return(c(x^6-3, 6*x^5))
}

CloseTo<-function(m,n){
  if (Mod(m - n) < 0.01){
    return(1)
  }else{
    return (0)
  }  
}

starting <- mat.or.vec(480,480)
root <- mat.or.vec(480,480)
repeattimes <- mat.or.vec(480,480)
Color <- mat.or.vec(480,480)
a <- seq(-1,1,length.out = 480)
b <- seq(-1,1,length.out = 480)
for (k in 1:480){
  for (j in 1:480){
    starting[k,j] <- complex(real = a[k], imaginary = b[j])
    Tolerance <- complex(real = 1E-05, imaginary = 1E-05)
    MaxnumberofIterations <- 10000
    Dev <- complex(real = 10000, imaginary = 10000)
    i <- 0
    x <- starting[k,j]
    while (i < MaxnumberofIterations && Mod(Dev) > Mod(Tolerance)) {
      fx <- Fomula(x)
      if (is.nan(fx[1]) || is.nan(fx[2])){
        cat("Functions or derivative not defined error.\n")
        break
      }
      x <- x - fx[1] / fx[2]
      Dev <- fx[1]
      i <- i + 1
      if (Mod(Dev) < Mod(Tolerance)){
        cat("\nFound the root point: ",x, "after ", i, "iterations by", k,j, "element." )
      }else{
        cat(paste("\nConvergence failure. Deviation: ",Dev, "after ", i,"iterations"))
      }
    }
    root[k,j] <- x
    repeattimes[k,j] <- i
  }
}

allroot <- numeric()
Judge <- 0
for (i in 1:480){
  for(j in 1:480){
    if (is.na(root[i,j]) == 0){
      if (length(allroot) == 0){
        allroot[1] <- root[1,1]
      }else{
        Judge <- 0
        for (k in 1:length(allroot)){
          if (CloseTo(root[i,j],allroot[k]) == 0){
            Judge <- Judge + 1
          }
        }
      }
      if (Judge == length(allroot)){
        allroot[length(allroot)+1] <- root[i,j]
      }
    }
  }
}

ThingstoPlot <- numeric()
for (i in 1:480){
  for (j in 1:480){
    Color[i,j]<-'black'
    cat(i,j,sep=" ")
    for (k in 1:length(allroot)){
      if (CloseTo(root[i,j],allroot[k])){
        Color[i,j] <- k  
      }
    }
    ThingstoPlot <- rbind(ThingstoPlot, c(Re(starting[i,j]),Im(starting[i,j]), Color[i,j]))
  }
}

ThingstoPlotG <- as.data.frame(ThingstoPlot, row.names, stringsAsFactors = F)
ThingstoPlotG$V1 <- as.numeric(ThingstoPlotG$V1)
ThingstoPlotG$V2 <- as.numeric(ThingstoPlotG$V2)
ggplot(ThingstoPlotG, aes(V1,V2,colour = V3))+geom_point()+scale_colour_manual(values = c(cm.colors(length(allroot))))

Color2 <- mat.or.vec(480,480)
alltimes <- numeric()
ThingstoPlot2 <- numeric()
for (i in 1:480){
  for(j in 1:480){
    if (!repeattimes[i,j] %in% alltimes){
      alltimes[length(alltimes) + 1] <- repeattimes[i,j]
    }
  }
}
for (i in 1:480){
  for (j in 1:480){
    Color2[i,j]<-'black'
    cat(i,j,sep=" ")
    for (k in 1:length(alltimes)){
      if (repeattimes[i,j] == alltimes[k]){
        Color2[i,j] <- k  
      }
    }
    ThingstoPlot2 <- rbind(ThingstoPlot2, c(Re(starting[i,j]),Im(starting[i,j]), Color2[i,j]))
  }
}


ThingstoPlotG2 <- as.data.frame(ThingstoPlot2, row.names, stringsAsFactors = F)
ThingstoPlotG2$V1 <- as.numeric(ThingstoPlotG2$V1)
ThingstoPlotG2$V2 <- as.numeric(ThingstoPlotG2$V2)
ggplot(ThingstoPlotG2, aes(V1,V2, color = V3))+geom_point()+scale_color_manual(values = c(heat.colors(length(alltimes))))