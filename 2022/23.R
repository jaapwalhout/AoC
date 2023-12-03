
library(data.table)

#-------
# part 1
#-------

d <- scan("2022/23_data_test.txt", what = character()) |>
  strsplit(split = "") |>
  do.call(what = rbind)
d



which(d == "#", arr.ind = TRUE)
