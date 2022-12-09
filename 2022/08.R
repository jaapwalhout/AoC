
library(matrixStats)

#-------
# part 1
#-------

d <- scan("2022/08_data.txt", what = character()) |>
  strsplit(split = "") |>
  type.convert()
d <- do.call(rbind, d)

d_b <- d[nrow(d):1,]
d_r <- d[, ncol(d):1]

top <- rbind(TRUE, colCummaxs(d)[-nrow(d),] < d[-1,])
lef <- cbind(TRUE, rowCummaxs(d)[,-ncol(d)] < d[,-1])
bot <- colCummaxs(d_b)[-nrow(d_b),] < d_b[-1,]
bot <- rbind(bot[nrow(bot):1,], TRUE)
rig <- rowCummaxs(d_r)[,-ncol(d_r)] < d_r[,-1]
rig <- cbind(rig[, ncol(rig):1], TRUE)

sum(top | lef | bot | rig)

#-------
# part 2
#-------

r <- matrix(0, nrow = nrow(d), ncol = ncol(d))

for ( i in 2:(nrow(d)-1) ) {
  for ( j in 2:(ncol(d)-1) ) {
    up <- d[(i-1):1, j] >= d[i,j]
    if (all(!up)) up <- length(up) else up <- which(up)[1]
    
    left <- d[i, (j-1):1] >= d[i,j]
    if (all(!left)) left <- length(left) else left <- which(left)[1]
    
    down <- d[(i+1):nrow(d), j] >= d[i,j]
    if (all(!down)) down <- length(down) else down <- which(down)[1]
    
    right <- d[i, (j+1):ncol(d)] >= d[i,j]
    if (all(!right)) right <- length(right) else right <- which(right)[1]
    
    r[i,j] <- down * up * left * right
  }
}

max(r)
