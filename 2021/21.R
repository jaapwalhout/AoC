
#---------
# part one
#---------

p1 <- 4
p2 <- 3

s1 <- 0
s2 <- 0

init_roll <- 0
n_roll <- 0

while (s1 < 1000 | s2 < 1000) {
  rolled <- init_roll + (1:3)
  rolled[rolled > 100] <- rolled[rolled > 100] - 100
  init_roll <- tail(rolled, 1)
  
  p1 <- (p1 + sum(rolled)) %% 10
  if (p1 == 0) p1 <- 10
  
  s1 <- s1 + p1
  
  n_roll <- n_roll + 3
  
  print(
    sprintf("player 1: rolls: %s -- pos: %s -- score: %s",
            paste(rolled, collapse = ", "), p1, s1)
  )
  
  if (s1 < 1000) {
    rolled <- init_roll + (1:3)
    rolled[rolled > 100] <- rolled[rolled > 100] - 100
    init_roll <- tail(rolled, 1)
    
    p2 <- (p2 + sum(rolled)) %% 10
    if (p2 == 0) p2 <- 10
    
    s2 <- s2 + p2
    
    n_roll <- n_roll + 3
    
    print(
      sprintf("player 2: rolls: %s -- pos: %s -- score: %s",
              paste(rolled, collapse = ", "), p2, s2)
    )
  } else break
}

n_roll * min(s1, s2)

#---------
# part two
#---------

