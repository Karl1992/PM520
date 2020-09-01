set.seed(80)
p <- numeric()

n <- 10000

for (j in 1:n){
  all_vitamins_remain <- sample(1:2, 60, replace = T) # I like 1 and my spouse likes 2.
  pick_percentage <- numeric()
  for (i in 1:15){
    today_pick <- sample(1:length(all_vitamins_remain), 4)
    
    all_vitamins_pick <- all_vitamins_remain[today_pick]
    all_vitamins_remain <- all_vitamins_remain[-today_pick]
    
    if (sum(all_vitamins_pick == 1) >= 2){
      pick_percentage[i] <- 2
      
    }else{
      pick_percentage[i] <- sum(all_vitamins_pick == 1)
    }
  }
  p[j] <- sum(pick_percentage) / 30
}

mean(p)

hist(p)
