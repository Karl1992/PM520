n <- 50

Urn_G <- function(finalballamount, starting_ball, black_weight){
  
  ball <- starting_ball # define the starting cituation
  
  while(length(ball) < finalballamount){
    
    pick <- sample(ball, 1, prob  = c(rep(1, length(ball)-1), black_weight)) # pick one ball from the urn
    
    if (pick == "0"){ # 0 is black
      
      ball <- ball[-which(ball == pick)] # pick the black ball
      
      pick2 <- sample(1:length(ball), 1) # pick a ball from the remaining balls
      
      ball[pick2] <- as.character(length(ball) + 1) # change the ball's color (since each iteration the length of ball will increase or remain the same, we'll never repeat the ball's color)
      
      ball <- c(ball, pick) # put back the black ball
      
    }else{
      ball <- ball[-length(ball)]
      
      ball <- c(ball, pick, "0") # else just add a ball with the picked color
      
    }
    
  }
  
  return(ball)
  
}
