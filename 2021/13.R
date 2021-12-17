
library(data.table)

#---------
# part one
#---------

# raw_input <- readLines("2021/13_data_test.txt")
raw_input <- readLines("2021/13_data.txt")
sepline <- which(raw_input == "")
inst <- raw_input[(sepline + 1):length(raw_input)]
inst <- fread(text = sub("fold along ", "", inst),
              sep = "=", header = FALSE)
d <- fread(text = raw_input[1:(sepline-1)], header = FALSE)

d[, (1:2) := lapply(.SD, '+', 1L)]
dims <- d[, sapply(.SD, max)]

m <- matrix(0L, nrow = dims[2], ncol = dims[1])
ix <- do.call(cbind, d[,2:1])
m[ix] <- 1

fold <- \(mat, i) {
  if (inst[i, V1 == "x"]) {
    x <- inst[i, V2]
    nc <- ncol(mat)
    m1 <- mat[,1:x]
    m2 <- mat[,nc:(x + 2)]
    res <- m1 + m2
  }
  if (inst[i, V1 == "y"]) {
    y <- inst[i, V2]
    nr <- nrow(mat)
    m1 <- mat[1:y,]
    if (y == nr/2) {
      m2 <- rbind(0L, mat[nr:(y + 2),])
    } else {
      m2 <- mat[nr:(y + 2),]
    }
    res <- m1 + m2
  }
  res[res > 0] <- 1L
  res
}

sum(fold(m, 1) == 1L)


#---------
# part one
#---------

for (ix in 1:nrow(inst)) {
  m <- fold(m, ix)
}

# for easier reading of the letters do:
m[m == 0L] <- ""

