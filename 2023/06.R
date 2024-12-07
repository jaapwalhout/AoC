
# d <- readLines("2023/06_data_test.txt")
d <- readLines("2023/06_data.txt")
times <- strsplit(d[1], "\\s+") |> unlist() |> as.integer()
times <- times[-1]
dists <- strsplit(d[2], "\\s+") |> unlist() |> as.integer()
dists <- dists[-1]

#-------
# part 1
#-------

calc_wins <- \(x, y) {
  speeds <- 1:x
  travel <- (rev(speeds) - 1) * speeds
  sum(travel > y)
}

mapply(calc_wins, x = times, y = dists) |> prod()

#-------
# part 2
#-------

time2 <- paste0(times, collapse = "") |> as.numeric()
dist2 <- paste0(dists, collapse = "") |> as.numeric()

calc_wins <- \(x, y) {
  speeds <- as.numeric(14:(x-14))
  travel <- rev(speeds) * speeds
  sum(travel > y)
}

calc_wins(time2, dist2)
