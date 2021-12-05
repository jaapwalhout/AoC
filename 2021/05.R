library(data.table)

#---------
# part one
#---------

# raw_input <- readLines("2021/05_data_test.txt")
raw_input <- readLines("2021/05_data.txt")

d05 <- setDT(tstrsplit(raw_input, split = ",|->", type.convert = TRUE))
setnames(d05, new = do.call(paste0, expand.grid(c("x","y"), 1:2)))

d05 <- d05[d05[, (x1 != x2) + (y1 != y2) < 2]]
d05[, (1:4) := lapply(.SD, `+`, 1L)]

m <-  max(as.matrix(d05))

mat <- matrix(0L, nrow = m, ncol = m)

for(i in 1:nrow(d05)) {
  x <- d05[[1]][i]:d05[[3]][i]
  y <- d05[[2]][i]:d05[[4]][i]
  mat[cbind(y, x)] <- mat[cbind(y, x)] + 1L
  # print(mat)
}

sum(mat >= 2)


#---------
# part two
#---------

# raw_input <- readLines("2021/05_data_test.txt")
raw_input <- readLines("2021/05_data.txt")

d05 <- setDT(tstrsplit(raw_input, split = ",|->", type.convert = TRUE))
setnames(d05, new = do.call(paste0, expand.grid(c("x","y"), 1:2)))

# d05 <- d05[d05[, (x1 != x2) + (y1 != y2) < 2]]
d05[, (1:4) := lapply(.SD, `+`, 1L)]

m <-  max(as.matrix(d05))

mat <- matrix(0L, nrow = m, ncol = m)

for(i in 1:nrow(d05)) {
  x <- d05[[1]][i]:d05[[3]][i]
  y <- d05[[2]][i]:d05[[4]][i]
  mat[cbind(y, x)] <- mat[cbind(y, x)] + 1L
  # print(mat)
}

sum(mat >= 2)
