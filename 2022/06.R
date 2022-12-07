
#-------
# part 1
#-------

d <- scan("2022/06_data.txt", what = character()) |>
  strsplit(split = "")
d

x <- d[[1]]

for (i in 1:length(x)) {
  if (length(unique(x[i:(i+3)])) == 4) {
    r <- i + 3
    break
  } else next
}
r

#-------
# part 2
#-------

d <- scan("2022/06_data.txt", what = character()) |>
  strsplit(split = "")
d

x <- d[[1]]

for (i in 1:length(x)) {
  if (length(unique(x[i:(i+13)])) == 14) {
    r <- i + 13
    break
  } else next
}
r

