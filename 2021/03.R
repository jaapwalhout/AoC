library(data.table)

#---------
# part one
#---------

d03 <- scan("2021/03_data.txt", what = character())
d03 <- setDT(tstrsplit(d03, split = "", type.convert = TRUE))

m <- 2^(11:0)

# option 1
g <- sum(round(colSums(d03) / nrow(d03)) * m)
e <- sum(round((nrow(d03) - colSums(d03)) / nrow(d03)) * m)

g * e

# option 2
g <- round(colSums(d03) / nrow(d03))
e <- abs(g - 1)

sum(g * m) * sum(e * m)

# option 3
g <- round(colSums(d03) / nrow(d03))
e <- abs(g - 1)

strtoi(paste0(g, collapse = ""), base = 2) * strtoi(paste0(e, collapse = ""), base = 2)


#---------
# part two
#---------

d03 <- scan("2021/03_data.txt", sep = "", what = character())
d03 <- setDT(tstrsplit(d03, split = "", type.convert = TRUE))

tmp <- copy(d03)

for (n in 1:ncol(d03)) {
  mc <- round(1L + sum(tmp[[n]])/nrow(tmp)) - 1L
  tmp <- tmp[tmp[[n]] == mc]
}

ox <- sum(unlist(tmp) * 2 ^ (11:0))

tmp <- copy(d03)

for (n in 1:ncol(d03)) {
  mc <- round((nrow(tmp) - sum(tmp[[n]]))/nrow(tmp))
  tmp <- tmp[tmp[[n]] == mc]
  if (nrow(tmp) == 1L) break
}

co <- sum(unlist(tmp) * 2 ^ (11:0))

ox * co

