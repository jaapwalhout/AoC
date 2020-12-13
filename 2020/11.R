
#---------
# part one
#---------

f <- function(d) {
  nr <- nrow(d)
  nc <- ncol(d)
  
  m <- matrix(rep(0L,nr*nc),nr,nc)
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      im <- setdiff((i-1L):(i+1L), c(0L,nr+1L))
      jm <- setdiff((j-1L):(j+1L), c(0L,nc+1L))
      if (d[i,j] == "L" & sum(d[im,jm] == "#") == 0L) {
        m[i,j] <- 1
      } else if (d[i,j] == "#" & sum(d[im,jm] == "#") > 4L) {
        m[i,j] <- 2
      }
    }
  }
  
  d[m == 1L] <- "#"
  d[m == 2L] <- "L"
  
  return(d)
}

d11 <- scan("2020/11_data.txt", what = character())
d11 <- strsplit(d11, "")
d11 <- do.call(rbind, d11)

test <- TRUE
prev <- matrix(rep("",nrow(d11)*ncol(d11)),nrow(d11))

while (test) {
  d11 <- f(d11)
  if ( all(d11 == prev) ) {
    test <- FALSE
    print(sum(d11 == "#"))
  } else {
    prev <- d11
  }
}



#---------
# part two
#---------

d11 <- scan(textConnection("L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"), what = character())

d11 <- strsplit(d11, "")
d11 <- do.call(rbind, d11)

f <- function(d) {
  nr <- nrow(d)
  nc <- ncol(d)
  
  m <- matrix(integer(nr*nc),nr,nc)
  
  ro <- row(d)
  co <- col(d)
  d_down <- co - ro
  d_up <- co + ro
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      c1 <- rev(setdiff(d[ro < i & co == j], "."))[1] %in% "#"
      c2 <- setdiff(d[ro > i & co == j], ".")[1] %in% "#"
      c3 <- rev(setdiff(d[ro == i & co < j], "."))[1] %in% "#"
      c4 <- setdiff(d[ro == i & co > j], ".")[1] %in% "#"
      c5 <- rev(setdiff(d[ro < i & co < j & d_down == d_down[i,j]], "."))[1] %in% "#"
      c6 <- setdiff(d[ro < i & co > j & d_up == d_up[i,j]], ".")[1] %in% "#"
      c7 <- rev(setdiff(d[ro > i & co < j & d_up == d_up[i,j]], "."))[1] %in% "#"
      c8 <- setdiff(d[ro > i & co > j & d_down == d_down[i,j]], ".")[1] %in% "#"
      
      s <- sum(c1, c2, c3, c4, c5, c6, c7, c8)
      
      if (d[i,j] == "L" & s == 0L) {
        m[i,j] <- 1
      } else if (d[i,j] == "#" & s >= 5L) {
        m[i,j] <- 2
      }
      
    }
  }
  
  d[m == 1L] <- "#"
  d[m == 2L] <- "L"
  
  return(d)
}

d11 <- scan("2020/11_data.txt", what = character())
d11 <- strsplit(d11, "")
d11 <- do.call(rbind, d11)

test <- TRUE
prev <- d11

while (test) {
  d11 <- f(d11)
  if ( all(d11 == prev) ) {
    test <- FALSE
    print(sum(d11 == "#"))
  } else {
    prev <- d11
  }
}


