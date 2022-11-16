
library(data.table)

#---------
# part one
#---------

d <- scan("2021/16_data.txt", what = character())

ref <- list("0" = c(0,0,0,0),
            "1" = c(0,0,0,1),
            "2" = c(0,0,1,0),
            "3" = c(0,0,1,1),
            "4" = c(0,1,0,0),
            "5" = c(0,1,0,1),
            "6" = c(0,1,1,0),
            "7" = c(0,1,1,1),
            "8" = c(1,0,0,0),
            "9" = c(1,0,0,1),
            "A" = c(1,0,1,0),
            "B" = c(1,0,1,1),
            "C" = c(1,1,0,0),
            "D" = c(1,1,0,1),
            "E" = c(1,1,1,0),
            "F" = c(1,1,1,1))
ref <- lapply(ref, as.integer)

e1 <- "D2FE28"
e2 <- "38006F45291200"
e3 <- "EE00D40C823060"
e4 <- "8A004A801A8002F478"
e5 <- "620080001611562C8802118E34"
e6 <- "C0015000016115A2E0802F182340"
e7 <- "A0016C880162017C3686B18A3D4780"

hex2bin <- \(x) {
  y <- strsplit(x, "")[[1]]
  ref[match(y, names(ref))] |> unlist() |> unname()
}

bin2num <- \(x) sum(x * 2^((length(x)-1):0), na.rm = TRUE)

b <- hex2bin("D2FE28")
bin2num(b[7:21][c(FALSE,rep(TRUE,4))])

digest_t4 <- \(x) {
  if (length(x) >= 5) {
    w <- which(x[c(TRUE,rep(FALSE,4))] == 0)[1]
    i <- 5 * w
    # n <- 1 + (5 * (0:(w - 1)))
    # tmp <- x[1:i]
    x <- x[-(1:i)]
  }
  return(x)
}

digest_bin <- function(x, ver_tot = 0) {
  if (length(x) == 0) return(list(x = x, ver_tot = ver_tot))
  
  ver <- bin2num(x[1:3])
  typ <- bin2num(x[4:6])
  x <- x[-(1:6)]
  
  ver_tot <- ver_tot + ver
  
  print(sprintf("Version total: %s", ver_tot))
  
  if (typ == 4) {
    x <- digest_t4(x)
  } else if (x[1] == 0L) {
    x <- x[-1]
    
    l_sub_pks <- bin2num(x[1:15])
    x <- x[-(1:15)]
    s <- x[1:l_sub_pks]
    
    print(sprintf("Length of sub packets: %s", l_sub_pks))
    
    while (length(s) > 0) {
      y <- digest_bin(x, ver_tot = ver_tot)
      x <- y$x
      ver_tot <- y$ver_tot
    }
    x <- x[-(1:l_sub_pks)]
  } else {
    x <- x[-1]
    n_sub_pks <- bin2num(x[1:11])
    
    print(sprintf("Number of sub packets: %s", n_sub_pks))
    x <- x[-(1:11)]
    
    for (n in seq.int(n_sub_pks)) {
      y <- digest_bin(x, ver_tot = ver_tot)
      x <- y$x
      ver_tot <- y$ver_tot
    }
  }
  return(list(x = x, ver_tot = ver_tot))
}



f1 <- \(x) hex2bin(x) |> digest_bin()

f1(e1)
f1(e2)
f1(e3)
f1(e4)
f1(e5)
f1(e6)
f1(e7)

f1(d)


#---------
# part two
#---------



