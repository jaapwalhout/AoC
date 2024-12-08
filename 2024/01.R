
# ------
# part 1
# ------

d <- scan("2024/01_data_test.txt", what = integer()) |> 
  matrix(ncol = 2, byrow = TRUE)

d <- scan("2024/01_data.txt", what = integer()) |> 
  matrix(ncol = 2, byrow = TRUE)

{sort(d[,1]) - sort(d[,2])} |>
  abs() |> sum()


# ------
# part 2
# ------

d <- scan("2024/01_data.txt", what = integer()) |> 
  matrix(ncol = 2, byrow = TRUE)

{sapply(d[,1], \(x) sum(x == d[,2])) * d[,1]} |> sum()
