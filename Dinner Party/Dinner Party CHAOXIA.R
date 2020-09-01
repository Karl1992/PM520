n <- 10
table <- rep(0,n)
gift <- rep(0,n)
table[1] = 1
gift[1] = 1
while (length(which(table == 0)) > 1){
  coin <- rbinom(1,1,prob = 0.5) # 0 is left, 1 is right
  if (coin == 0){
    if (which(gift == 1) == 1){
      gift[n] = 1
      gift[1] = 0
      table[n] = 1
    }
    else {
      flag <- which(gift == 1)
      gift[flag - 1] = 1
      gift[flag] = 0
      table[flag - 1] = 1
    }
  }
  else {
    if (which(gift == 1) == n){
      gift[1] = 1
      gift[n] = 0
    }
    else {
      flag <- which(gift == 1)
      gift[flag + 1] = 1
      gift[flag] = 0
      table[flag + 1] = 1
    }
  }
}
winner <- which(table == 0)