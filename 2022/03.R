
#-------
# part 1
#-------

d <- scan("2022/03_data.txt", what = character())

d1 <- substr(d, 1, nchar(d)/2) |> strsplit(split = "")
d2 <- substring(d, (nchar(d)/2) + 1) |> strsplit(split = "")

mapply(intersect, d1, d2) |> 
  match(c(letters, LETTERS)) |> 
  sum()

#-------
# part 2
#-------

d <- scan("2022/03_data.txt", what = character())

d <- split(d, rep(1:(length(d)/3), each = 3))

f <- \(x) Reduce(intersect, x)

lapply(d, strsplit, split = "") |>
  sapply(f) |>
  match(c(letters, LETTERS)) |> 
  sum()
