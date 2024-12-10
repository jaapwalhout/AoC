options(scipen = 99)

# ------
# part 1
# ------

d <- scan("2024/04_data.txt", what = character())
m <- strsplit(d, "") |> do.call(what = rbind)

direcs <- list(
  N = matrix(c(0,0,0,0,0:-3), ncol = 2),
  NW = matrix(c(0:3,0:-3), ncol = 2),
  W = matrix(c(0:3,0,0,0,0), ncol = 2),
  SW = matrix(c(0:3,0:3), ncol = 2),
  S = matrix(c(0,0,0,0,0:3), ncol = 2),
  SE = matrix(c(0:-3,0:3), ncol = 2),
  E = matrix(c(0:-3,0,0,0,0), ncol = 2),
  NE = matrix(c(0:-3,0:-3), ncol = 2)
)

is.xmas <- \(x) paste0(x, collapse = "") == "XMAS"

iw <- which(m == "X", arr.ind = TRUE)

sapply(1:nrow(iw), \(nr) {
  sapply(direcs, \(x) {
    is <- sweep(x, 2, iw[nr,], '+')
    
    if (any(is < 1) | any(is > max(dim(m)))) {
      FALSE
    } else {
      is.xmas(m[is])
    }
  })
}) |> sum()
  
  
# ------
# part 2
# ------

d <- scan("2024/04_data.txt", what = character())
m <- strsplit(d, "") |> do.call(what = rbind)

iw <- which(m == "A", arr.ind = TRUE)

direcs <- list(
  d1 = cbind(-1:1,-1:1),
  d2 = cbind(-1:1,1:-1)
)

is.mas <- \(x) paste0(x, collapse = "") %in% c("MAS","SAM")

s <- lapply(1:nrow(iw), \(nr) {
  sapply(direcs, \(x) {
    is <- sweep(x, 2, iw[nr,], '+')
    
    if (any(is < 1) | any(is > max(dim(m)))) {
      FALSE
    } else {
      is.mas(m[is])
    }
  }) |> sum()
})
sum(s == 2)

