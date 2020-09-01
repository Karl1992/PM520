n <- 50

Urn <- function(finalball, starting_ball){
  ball <- starting_ball
  while(length(ball) < finalball){
    pick <- sample(ball, "1")
    if (pick == "0"){
      ball <- ball[-which(ball == pick)]
      pick2 <- sample(1:length(ball),1)
      ball[pick2] <- as.character(length(ball) + 1)
      ball <- c(ball, pick)
    }else{
      ball <- c(ball, pick)
    }
  }
  return(ball)
}

end <- numeric()
repeattimes <- 10000
for (i in 1:repeattimes) {
  end[i] <- dim(table(Urn(51, c("1","0")))) - 1 
}

hist(end)

