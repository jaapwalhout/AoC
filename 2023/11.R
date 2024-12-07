
d <- scan(file = "2023/11_data_test.txt", what = character()) |>
  strsplit(split = "") |>
  do.call(what = rbind)
# d <- scan(file = "2023/11_data.txt", what = character()) |>
#   strsplit(split = "") |>
#   do.call(what = rbind)

w <- which(d == "#", arr.ind = TRUE)

n1 <- nrow(w) - 1


#-------
# part 1
#-------

# solution 1
rep_j <- (colSums(d == ".") == nrow(d)) + 1L
rep_i <- (rowSums(d == ".") == ncol(d)) + 1L

d <- d[rep(seq_along(rep_i), times = rep_i), rep(seq_along(rep_j), times = rep_j)]

w <- which(d == "#", arr.ind = TRUE)

n1 <- nrow(w) - 1

m <- matrix(0, n1, n1)

for (i in 1:n1) {
  if (i == n1) {
    s <- w[(i:n1) + 1,] - w[i,]
    m[i,i:n1] <- sum(abs(s))
  } else {
    s <- sweep(w[(i:n1) + 1,], 2, w[i,]) |> abs()
    m[i,i:n1] <- rowSums(s)
  }
}

sum(m)

# solution 2
# (same as part 2, just a different number of replications of empty columns/rows)

f <- 2

k <- (colSums(d == ".") == nrow(d)) |> which()
r <- (rowSums(d == ".") == ncol(d)) |> which()

v <- 0

for (i in 1:n1) {
  for (j in (i:n1) + 1) {
    s <- {w[j,] - w[i,]} |> abs() |> sum()
    nr <- min(w[j,][1], w[i,][1]) < r & max(w[j,][1], w[i,][1]) > r
    nk <- min(w[j,][2], w[i,][2]) < k & max(w[j,][2], w[i,][2]) > k
    v <- v + s + ((sum(nr) + sum(nk)) * (f - 1))
  }
}
v


#-------
# part 2
#-------

f <- 1e6

k <- (colSums(d == ".") == nrow(d)) |> which()
r <- (rowSums(d == ".") == ncol(d)) |> which()

v <- 0

for (i in 1:n1) {
  for (j in (i:n1) + 1) {
    s <- {w[j,] - w[i,]} |> abs() |> sum()
    nr <- min(w[j,][1], w[i,][1]) < r & max(w[j,][1], w[i,][1]) > r
    nk <- min(w[j,][2], w[i,][2]) < k & max(w[j,][2], w[i,][2]) > k
    v <- v + s + ((sum(nr) + sum(nk)) * (f - 1))
  }
}
v

