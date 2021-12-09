
library(data.table)
library(raster)

#---------
# part one
#---------

# d <- scan("2021/09_data_test.txt", what = character())
d <- scan("2021/09_data.txt", what = character())

d <- tstrsplit(d, split = "", type.convert = TRUE)
d <- as.matrix(setDT(d))

nc <- ncol(d)
nr <- nrow(d)

m1 <- rbind(rep(TRUE,nc), d[2:nr,] < d[1:(nr-1),])
m2 <- rbind(d[1:(nr-1),] < d[2:nr,], rep(TRUE,nc))
m3 <- cbind(rep(TRUE,nr), d[,2:nc] < d[,1:(nc-1)])
m4 <- cbind(d[,1:(nc-1)] < d[,2:nc], rep(TRUE,nr))

sum(d[m1 & m2 & m3 & m4] + 1)


#---------
# part two
#---------

# d <- scan("2021/09_data_test.txt", what = character())
d <- scan("2021/09_data.txt", what = character())

d <- tstrsplit(d, split = "", type.convert = TRUE)
d <- as.matrix(setDT(d))

dr <- raster(d)
dr[dr != 9] <- 1
dr[dr == 9] <- 0

basins <- clump(dr, directions = 4)
# matrix(basins[], nrow = 5, byrow = TRUE)
v <- sort(table(basins[]), decreasing = TRUE)[1:3]

# option 1
Reduce('*', v)

# option 2
prod(v)
