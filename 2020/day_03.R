library(data.table)

#---------
# part one
#---------

d3 <- scan(file = "day_3_data.txt", what = character())
d3 <- strsplit(d3, "")
d3 <- do.call(rbind.data.frame, d3)

d3 <- rep(d3, ceiling((323*3)/31))
d3 <- as.data.frame.list(d3)
names(d3) <- paste0("V", seq_along(d3))

l <- c(list(c(1L,1L)), rep(list(c(1L,3L)), 322))
r <- Reduce(`+`, l, accumulate = TRUE)
ai <- do.call(rbind, r)

sum(d3[ai] == "#")

#---------
# part two
#---------

d3 <- scan(file = "day_3_data.txt", what = character())
d3 <- strsplit(d3, "")
d3 <- do.call(rbind.data.frame, d3)

slopes <- list(c(1L,1L), c(1L,3L), c(1L,5L), c(1L,7L), c(2L,1L))
f <- function(data, slope) {
  d <- rep(data, ceiling((nrow(data) * slope[[2]])/ncol(data)))
  d <- as.data.frame.list(d)
  names(d) <- paste0("V", seq_along(d))

  l <- c(list(c(1L,1L)), rep(list(slope), (322 / slope[[1]])))
  r <- Reduce(`+`, l, accumulate = TRUE)
  ai <- do.call(rbind, r)

  sum(d[ai] == "#")
}

s <- sapply(slopes, function(x) f(d3, x))

s
cumprod(s)
Reduce('*', as.numeric(s))
