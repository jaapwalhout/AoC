
#-------
# part 1
#-------

# d <- readLines("2023/04_data_test.txt")
d <- readLines("2023/04_data.txt")

winnums <- sub(".*\\:(.*)\\|.*", "\\1", d) |>
  trimws() |>
  strsplit(split = "\\s+") |>
  lapply(as.integer)

mynums <- sub(".*\\|(.*)", "\\1", d) |>
  trimws() |>
  strsplit(split = "\\s+") |>
  lapply(as.integer)

mapply(\(x, y) {
  s <- sum(y %in% x)
  if (s != 0) 2 ^ (s - 1) else 0
}, x = winnums, y = mynums) |> sum()

#-------
# part 2
#-------

extra <- mapply(\(x, y) {
  sum(y %in% x)
}, x = winnums, y = mynums)

cards <- rep(1L, length(d))

for(i in seq_along(extra)) {
  ix <- seq(extra[i]) + i
  ix <- ix[ix <= length(d)]
  if (extra[i] > 0) {
    cards[ix] <- cards[ix] + cards[i]
  }
}

sum(cards)
