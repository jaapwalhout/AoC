
options(scipen = 999)
library(data.table)

# ------
# part 1
# ------

# d <- scan("2024/13_data_test.txt", what = character(), sep = "\n")
d <- scan("2024/13_data.txt", what = character(), sep = "\n")

g <- gregexpr("\\d+", d)
r <- regmatches(d, g) |> lapply(as.numeric) |> do.call(what = rbind)

n <- length(r)/6
X <- split(r[,1], rep(1:n, each = 3))
Y <- split(r[,2], rep(1:n, each = 3))

lijst <- vector(mode = "list", length = n)

for (i in seq_along(X)) {
  Xi <- X[[i]]
  Yi <- Y[[i]]
  
  v <- 1:100 # nmax
  
  mA <- outer(v*Xi[1], v*Xi[2], '+')
  wA <- which(mA == Xi[3], arr.ind = TRUE) |> as.data.table()
  
  mB <- outer(v*Yi[1], v*Yi[2], '+')
  wB <- which(mB == Yi[3], arr.ind = TRUE) |> as.data.table()
  lijst[[i]] <- fintersect(wA, wB)
}

il <- sapply(lijst, nrow) > 0

costs <- lapply(lijst[il], '*', c(3,1)) |> sapply(sum)
sum(costs)




