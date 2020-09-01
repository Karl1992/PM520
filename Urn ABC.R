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
      
      ball <- c(ball, pick, "0") # else just add a ball with the picked color and keep the black ball at last of the row
      
    }
    
  }
  
  return(ball)
  
}

# The number of different colors in the urn was 5

NoReps <- 10000

Howmanycolorneeded <- 5

Maxweight <- 20

Noofballs <- 20

Acceptedweights <- rep(-9, NoReps)

for (i in 1:NoReps) {
  HowManyColorsObserved<- -9
  while (HowManyColorsObserved != Howmanycolorneeded) {
    # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
    ThisWeight <- sample(Maxweight,1)
    Final_state <- Urn_G(Noofballs, c(1,1,0), ThisWeight) # simulate the urn using this weight
    HowManyColorsObserved <- length(table(Final_state)) - 1
  }
  Acceptedweights[i]<-ThisWeight
}

hist(Acceptedweights, breaks = Maxweight, main = "Number of balls")

# There are 7 balls of the commonest color
Amountofmostcommonball <- 7

for (i in 1:NoReps) {
  AmountofmostcommonballObserved <- -9
  while (AmountofmostcommonballObserved != Amountofmostcommonball) {
    # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
    ThisWeight <- sample(Maxweight,1)
    Final_state <- Urn_G(Noofballs, c(1,1,0), ThisWeight) # simulate the urn using this weight
    AmountofmostcommonballObserved <- max(table(Final_state))
  }
  Acceptedweights[i]<-ThisWeight
}

hist(Acceptedweights, breaks = Maxweight, main = "Commonest ball")

# The number of times black ball was drawwn was 7

Urn_G_prime <- function(finalballamount, starting_ball, black_weight){
  
  ball <- starting_ball # define the starting cituation
  
  black_pick <- 0
  
  while(length(ball) < finalballamount){
    
    pick <- sample(ball, 1, prob  = c(rep(1, length(ball)-1), black_weight)) # pick one ball from the urn
    
    if (pick == "0"){ # 0 is black
      
      ball <- ball[-which(ball == pick)] # pick the black ball
      black_pick <- black_pick + 1
      
      pick2 <- sample(1:length(ball), 1) # pick a ball from the remaining balls
      
      ball[pick2] <- as.character(length(ball) + 1) # change the ball's color (since each iteration the length of ball will increase or remain the same, we'll never repeat the ball's color)
      
      ball <- c(ball, pick) # put back the black ball
      
    }else{
      ball <- ball[-length(ball)]
      
      ball <- c(ball, pick, "0") # else just add a ball with the picked color and keep the black ball at last of the row
      
    }
    
  }
  
  return(black_pick)
}

black_pick_needed <- 7

for (i in 1:NoReps) {
  black_pick_observed <- -9
  while (black_pick_observed != black_pick_needed) {
    # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
    ThisWeight <- sample(Maxweight,1)
    black_pick_observed <- Urn_G_prime(Noofballs, c(1,1,0), ThisWeight)
  }
  Acceptedweights[i]<-ThisWeight
}

hist(Acceptedweights, breaks = Maxweight, main = "black pick")

# Combination

NoReps <- 10000

Howmanycolorneeded <- 5

Amountofmostcommonball <- 7

Maxweight <- 20

Noofballs <- 20

Acceptedweights <- rep(-9, NoReps)

for (i in 1:NoReps) {
  HowManyColorsObserved<- -9
  AmountofmostcommonballObserved <- -9
  while (HowManyColorsObserved != Howmanycolorneeded | AmountofmostcommonballObserved != Amountofmostcommonball) {
    # Sample a weight, ThisWeight, for the mutation ball, from Unif(0,MaxWeight). Then...
    ThisWeight <- sample(Maxweight,1)
    Final_state <- Urn_G(Noofballs, c(1,1,0), ThisWeight) # simulate the urn using this weight
    HowManyColorsObserved <- length(table(Final_state)) - 1
    AmountofmostcommonballObserved <- max(table(Final_state))
  }
  Acceptedweights[i]<-ThisWeight
}

hist(Acceptedweights, breaks = Maxweight, main = "Combination")