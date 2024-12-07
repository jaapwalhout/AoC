
# d <- scan(file = "2023/09_data_test.txt", what = character(), sep = "\n") |>
#   strsplit(split = " ") |>
#   lapply(as.numeric)

d <- scan(file = "2023/09_data.txt", what = character(), sep = "\n") |>
  strsplit(split = " ") |>
  lapply(as.numeric)

#-------
# part 1
#-------

get_hist <- \(x) {
  tmp <- diff(x)
  diffs <- tail(tmp, 1)
  while(!all(tmp == 0)) {
    tmp <- diff(tmp)
    diffs <- c(diffs, tail(tmp, 1))
  }
  sum(diffs) + tail(x, 1)
}

sapply(d, get_hist) |> sum()


#-------
# part 2
#-------

get_hist <- \(x) {
  h <- head(x, 1)
  tmp <- diff(x)
  diffs <- head(tmp, 1)
  while(!all(tmp == 0)) {
    tmp <- diff(tmp)
    diffs <- c(diffs, head(tmp, 1))
  }
  rd <- rev(diffs)
  ix <- head(seq_along(rd), -1)
  for (i in ix) {
    i1 <- i + 1
    rd[i1] <- rd[i1] - rd[i]
  }
  h - rd[i1]
}

sapply(d, get_hist) |> sum()

