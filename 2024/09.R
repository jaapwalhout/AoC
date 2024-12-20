
options(scipen = 99)

# ------
# part 1
# ------

# test data
blockcoding <- "2333133121414131402"

b <- strsplit(blockcoding, split = "") |> unlist() |> as.integer()
ib <- seq_along(b)
i <- {ib %% 2} |> as.logical()
maxi <- sum(i) - 1

m <- matrix(
  c(0:maxi, rep(NA, sum(i))),
  ncol = maxi + 1,
  byrow = TRUE
) |> c()
m <- m[ib]
m

v <- rep(m,b)

while(sum(is.na(v)) > 0) {
  if (is.na(tail(v, 1))) {
    v <- head(v, -1)
  } else {
    ix <- which(is.na(v))[1]
    v[ix] <- tail(v, 1)
    v <- head(v, -1)
  }
  print(v)
}

{(seq_along(v) - 1) * v} |> sum()

# puzzle data
d <- scan(file = "2024/09_data.txt", what = character())

b <- strsplit(d, split = "") |> unlist() |> as.integer()
ib <- seq_along(b)
i <- {ib %% 2} |> as.logical()
maxi <- sum(i) - 1

m <- matrix(
  c(0:maxi, rep(NA, sum(i))),
  ncol = maxi + 1,
  byrow = TRUE
) |> c()
m <- m[ib]

v <- rep(m, b)

while(sum(is.na(v)) > 0) {
  if (is.na(tail(v, 1))) {
    v <- head(v, -1)
  } else {
    ix <- which(is.na(v))[1]
    v[ix] <- tail(v, 1)
    v <- head(v, -1)
  }
}

{(seq_along(v) - 1) * v} |> sum()

# much much faster solution
v2 <- rep(m, b)

cs_vol <- cumsum(rev(!is.na(v2))) |> rev()
cs_leeg <- cumsum(is.na(v2))
split_ix <- which(cs_leeg == cs_vol)

vulling <- v2[(split_ix:length(v2))] |> na.omit() |> rev()
v2 <- v2[1:split_ix]
v2[is.na(v2)] <- vulling

{(seq_along(v) - 1) * v} |> sum()


# ------
# part 2
# ------

# test data
d <- "2333133121414131402"

# puzzle data
d <- scan(file = "2024/09_data.txt", what = character())

b <- strsplit(d, split = "") |> unlist() |> as.integer()
ib <- seq_along(b)
i <- {ib %% 2} |> as.logical()
maxi <- sum(i) - 1

m <- matrix(
  c(0:maxi, rep(NA, sum(i))),
  ncol = maxi + 1,
  byrow = TRUE
) |> c()
m <- m[ib]

v <- rep(m, b)

vl <- split(v, rep(seq_along(b[b!=0]), b[b!=0]))

nas <- sapply(vl, \(x) all(is.na(x)))
nna <- sapply(vl, \(x) sum(is.na(x)))

w <- which(!nas)

for (i in rev(w)) {
  vi <- vl[[i]]
  li <- length(vi)
  
  nna <- sapply(vl, \(x) sum(is.na(x)))
  ina <- nna[nna >= li] |> names() |> as.integer()
  
  if (i > ina[1] & length(ina) > 0) {
    if (li <= nna[ina[1]]) {
      wi <- which(is.na(vl[[ ina[1] ]])) |> head(li)
      vl[[ ina[1] ]][ wi ] <- vi
      vl[[i]] <- rep(NA, li)
    }
  } 
}

vlu <- unlist(vl)
{(seq_along(vlu) - 1) * vlu} |> sum(na.rm = TRUE)

