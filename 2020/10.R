
#---------
# part one
#---------

d10 <- scan("2020/10_data.txt")

d <- diff(c(0, sort(d10), max(d10) + 3))

prod(table(d))



#---------
# part two
#---------

# set scipen option to a large number to prevent scientific printing
options(scipen = 999)

# read the data
d10 <- scan("2020/10_data.txt")
d10 <- c(0, sort(d10), max(d10) + 3)

# explore possible routes with a matrix
# - use 'outer' to get a difference matrix
# - transform the matrix tot ones where the differences meet the
#   criteria and to zero where they do not
m1 <- outer(tail(d10, -1), head(d10, -1), '-')
m2 <- +(m1 >= 1 & m1 <= 3)

# use rle to get the sequences of ones and threes of the
# differences of the data
r <- rle(diff(d10))

# with sequences of
# - 4 ones, there are 7 possible routes
# - 3 ones, there are 4 possible routes
# - 2 ones, there are 2 possible routes
# - 1 ones, there is 1 possible route
prod(c(7,4,2,1)[match(r$lengths[r$values == 1], 4:1)])

