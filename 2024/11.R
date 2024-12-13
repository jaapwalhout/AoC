
options(scipen = 999)

# ------
# part 1
# ------

# test data
d <- c(125, 17)

# puzzle data
d <- c(77, 515, 6779622, 6, 91370, 959685, 0, 9861)


f <- \(x) {
  if (x == 0) {
    return(1)
  } else if ((nchar(x) %% 2) == 0) {
    nc <- nchar(x)
    return(c(substr(x, 1, nc/2), substr(x, (nc/2)+1, nc)) |> as.numeric())
  } else {
    return(x * 2024)
  }
}

for (i in 1:25) {
  d <- sapply(d, f, USE.NAMES = FALSE) |> unlist()
}
length(d)


# ------
# part 2
# ------

d <- c(77, 515, 6779622, 6, 91370, 959685, 0, 9861)

v <- rle(d)$values
l <- rle(d)$lengths

for (i in 1:75) {
  print(i)
  ld <- lapply(v, f)
  l2 <- rep(l, lengths(ld))
  v2 <- unlist(ld)
  nl <- tapply(l2, v2, sum)
  v <- as.numeric(names(nl))
  l <- unname(nl)
}

sum(l)


# ------
# part 2 (first attempt which should work, but consumed too much memory)
# ------

d <- c(77, 515, 6779622, 6, 91370, 959685, 0, 9861)

f <- \(x) {
  i1 <- x == 0
  i2 <- (nchar(x) %% 2) == 0 & x != 0
  i3 <- !(i1 | i2)
  
  x1 <- NA^x[i1]
  
  x2 <- x[i2]
  nc <- nchar(x2)
  x2 <- c(substr(x2, 1, nc/2), substr(x2, (nc/2)+1, nc)) |> as.numeric()
  
  x3 <- x[i3] * 2024
  
  return(c(x1, x2, x3))
}

for (i in 1:75) {
  d <- f(d)
  print(i)
}

length(d)


