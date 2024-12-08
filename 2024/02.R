
"2024/02_data_test.txt"
"2024/02_data.txt"

# ------
# part 1
# ------

d <- readLines("2024/02_data.txt") |> 
  strsplit(split = " ") |>
  lapply(as.integer)

f <- \(x) {
  all(abs(x) <= 3) & (all(sign(x) == -1) | all(sign(x) == 1))
}

lapply(d, diff) |>
  sapply(f) |>
  sum()


# ------
# part 2
# ------

d <- readLines("2024/02_data.txt") |> 
  strsplit(split = " ") |>
  lapply(as.integer)

f <- \(x) {
  all(abs(x) <= 3) & (all(sign(x) == -1) | all(sign(x) == 1))
}

f2 <- \(x) {
  di <- diff(x)
  is_safe <- f(di)
  
  si <- sign(di)
  ta <- table(si)
  
  if (!is_safe & length(ta) == 2) {
    i <- ta |> which.min() |> names() |> as.integer()
    w <- which(si == i)
    is_safe <- diff(x[-w]) |> f()
  }
  
  return(is_safe)
}

sapply(d, f2) |> sum()


