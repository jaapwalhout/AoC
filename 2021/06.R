
#---------
# part one
#---------

# d06 <- scan("2021/06_data_test.txt", sep = ",", what = integer())
d06 <- scan("2021/06_data.txt", sep = ",", what = integer())

res <- vector(mode = "list", length = 81)
res[[1]] <- d06

for (i in 1:80) {
  res[[i+1]] <- res[[i]] - 1L
  s <- sum(res[[i]] == 0L)
  if(s > 0) {
    res[[i+1]][res[[i]] == 0L] <- 6L
    res[[i+1]] <- c(res[[i+1]], rep(8, s))
  }
}
length(res[[81]])

# answer: 352151


#---------
# part two
#---------

options(scipen = 99)

# d06 <- scan("2021/06_data_test.txt", sep = ",", what = integer())
d06 <- scan("2021/06_data.txt", sep = ",", what = integer())

nums <- as.numeric(0:8)
counts <- as.numeric(rep(0, 9))

tmp_counts <- c(table(d06))

counts[match(names(tmp_counts), nums)] <- unname(tmp_counts)

for (i in 1:256) {
  tmp_counts <- counts[c(2:9,1)]
  tmp_counts[7] <- tmp_counts[7] + counts[1]
  counts <- tmp_counts
}

sum(counts)

# answer: 1601616884019
