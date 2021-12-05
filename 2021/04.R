library(data.table)

#---------
# prepare
#---------

# raw_input <- readLines("2021/04_data_test.txt")
raw_input <- readLines("2021/04_data.txt")

draws <- as.integer(strsplit(raw_input[1], ",")[[1]])

# function to get the board from the input
get_boards <- function(x) {
  n <- floor(length(x)/6)
  Map(function(l1,l2) as.matrix(read.table(text = x[l1:l2])),
      l1 = 3+(0:(n-1))*6,
      l2 = 7+(0:(n-1))*6)
}

boards <- get_boards(raw_input)


#---------
# part one
#---------

boards_check <- replicate(floor(length(raw_input)/6), matrix(0L, nrow = 5, ncol = 5), simplify = FALSE)

for(i in seq_along(draws)) {
  for(j in seq_along(boards)) {
    w <- which(boards[[j]] == draws[i], arr.ind = TRUE)
    boards_check[[j]][w] <- 1L
    rs <- rowSums(boards_check[[j]]) == 5L
    cs <- colSums(boards_check[[j]]) == 5L
    if(any(rs) | any(cs)) {
      if(any(rs)) r <- sum(boards[[j]][boards_check[[j]] == 0L]) * draws[i]
      if(any(cs)) r <- sum(boards[[j]][boards_check[[j]] == 0L]) * draws[i]
      break
    }
  }
  if(any(rs) | any(cs)) break
}

r

#---------
# part two
#---------

boards <- get_boards(raw_input)

boards_check <- replicate(floor(length(raw_input)/6), matrix(0L, nrow = 5, ncol = 5), simplify = FALSE)

wins <- rep(NA, length(boards))

for(j in seq_along(boards)) {
  for(i in seq_along(draws)) {
    m <- matrix(boards[[j]] %in% draws[1:i], nrow = 5)
    rs <- any(rowSums(m) == 5L)
    cs <- any(colSums(m) == 5L)
    if (rs | cs) {
      wins[j] <- i
      break
    }
  }
}

sum(setdiff(boards[[which.max(wins)]], draws[1:max(wins)])) * draws[max(wins)]

