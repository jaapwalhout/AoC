
library(data.table)
# library(raster)

#---------
# part one
#---------

# d <- scan("2021/11_data_test.txt", what = character())
d <- scan("2021/11_data.txt", what = character())

d <- tstrsplit(d, split = "", type.convert = TRUE)
d <- as.matrix(setDT(d))

eg <- expand.grid(-1:1,-1:1)
eg <- eg[!rowSums(eg == 0) == 2,]

f1 <- \(x, steps) {
  n <- 0
  for (s in 1:steps) {
    x <- x + 1
    while (any(x > 9, na.rm = TRUE)) {
      w <- which(x > 9, arr.ind = TRUE)
      for (i in 1:nrow(w)) {
        ix <- sweep(eg, 2, w[i,], "+")
        ix <- ix[!rowSums(ix < 1 | ix > 10),]
        ix <- do.call(cbind, ix)
        x[ix] <- x[ix] + 1L
      }
      x[w] <- NA
    }
    n <- n + sum(is.na(x))
    x[is.na(x)] <- 0L
  }
  return(n)
}

f1(d, 100)

# reminder: try also with raster


#---------
# part two
#---------

# d <- scan("2021/11_data_test.txt", what = character())
d <- scan("2021/11_data.txt", what = character())

d <- tstrsplit(d, split = "", type.convert = TRUE)
d <- as.matrix(setDT(d))


f2 <- \(x) {
  l <- length(x)
  s <- 1
  repeat {
    x <- x + 1
    while (any(x > 9, na.rm = TRUE)) {
      w <- which(x > 9, arr.ind = TRUE)
      for (i in 1:nrow(w)) {
        ix <- sweep(eg, 2, w[i,], "+")
        ix <- ix[!rowSums(ix < 1 | ix > 10),]
        ix <- do.call(cbind, ix)
        x[ix] <- x[ix] + 1L
      }
      x[w] <- NA
    }
    if (l == sum(is.na(x))) break
    x[is.na(x)] <- 0L
    s <- s + 1
  }
  return(s)
}

f2(d)
