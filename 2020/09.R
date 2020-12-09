
library(data.table)

#---------
# part one
#---------

d9 <- fread("2020/09_data.txt", header = FALSE, colClasses = "numeric")

shifted <- as.data.table(t(d9[, shift(V1, n = 1:25)]))

i <- which(!mapply(function(x, y) any(x == y),
                   x = lapply(shifted, function(x) outer(x, x, '+')),
                   y = d9$V1,
                   USE.NAMES = FALSE))

invalid <- d9[i[1], V1]


#---------
# part two
#---------

not_found_yet <- TRUE
window_size <- 2L

while( not_found_yet ) {
  i <- which(rowSums(d9[, shift(V1, 0:(window_size - 1))]) == invalid)
  if ( length(i) > 0 ) {
    not_found_yet <- FALSE
    nums <- d9[(i-(window_size - 1)):i, range(V1)]
  } else {
    window_size <- window_size + 1L
  }
}

sum(nums)
