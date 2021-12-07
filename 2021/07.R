
#---------
# part one
#---------

# d <- scan("2021/07_data_test.txt", sep = ",", what = integer())
d <- scan("2021/07_data.txt", sep = ",", what = integer())

m <- abs(outer(d, d, "-"))
sum(abs(d - d[which.min(colSums(m))]))


#---------
# part two
#---------

# d <- scan("2021/07_data_test.txt", sep = ",", what = integer())
d <- scan("2021/07_data.txt", sep = ",", what = integer())

m <- abs(outer(d, 1:max(d), "-"))
i0 <- m == 0

# option 1

for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j] <- sum(1:m[i,j])
  }
}

m[i0] <- 0

sum(m[,which.min(colSums(m))])

# option 2

v <- c(m)
s <- sapply(v, function(x) sum(1:x))
s[c(i0)] <- 0

m <- matrix(s, nrow = nrow(m))

sum(m[,which.min(colSums(m))])

# option 3

s <- sapply(min(d):max(d), function(x) {
  y <- abs(d-x)
  s <- sapply(y[y!=0], function(i) sum(seq(i)))
  sum(s)
} )

min(s)
