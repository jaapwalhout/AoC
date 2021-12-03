library(data.table)

#---------
# part one
#---------

d <- scan(file = "2021/01_data.txt", what = integer())
sum(diff(d) > 0)

#---------
# part two
#---------

sum(rowSums(setDT(shift(d, 0:2))) > rowSums(setDT(shift(d, 1:3))), na.rm = TRUE)

sum(frollsum(d, 3) > frollsum(shift(d), 3), na.rm = TRUE)

roll_sum <- function(x, n) {
  lx <- length(x)
  cx <- cumsum(x)
  cx[n:lx] - c(0, cx[1:(lx-n)])
}
sum(diff(roll_sum(d, 3)) > 0)

