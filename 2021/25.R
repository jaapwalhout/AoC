
#---------
# part one
#---------

cucumbers <- scan("2021/25_data_test.txt", what = character()) |>
  strsplit(split = "") |>
  do.call(what = rbind)

left