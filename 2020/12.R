
library(data.table)

#---------
# part one
#---------

d12 <- read.fwf("2020/12_data.txt", widths = c(1,4))#[1:15,]

ew <- 0
ns <- 0
di <- "E"
dirs <- c("N" = 0, "W" = 90, "S" = 180, "E" = 270)

rotateL <- function(i) {
  names(dirs[dirs == abs((dirs[di] - d12[i, "V2"]) %/% 90) * 90])
}

rotateR <- function(i) {
  tmp <- ((dirs[di] + d12[i, "V2"]) %/% 90) * 90
  tmp <- ifelse(tmp >= 360, tmp - 360, tmp)
  names(dirs[dirs == tmp])
}

for (i in seq(nrow(d12))) {
  switch(d12[i, "V1"],
         N = ns <- ns + d12[i, "V2"],
         S = ns <- ns - d12[i, "V2"],
         E = ew <- ew + d12[i, "V2"],
         W = ew <- ew - d12[i, "V2"],
         L = di <- rotateL(i),
         R = di <- rotateR(i),
         F = fcase(di == "E", ew <- ew + d12[i, "V2"],
                   di == "W", ew <- ew - d12[i, "V2"],
                   di == "N", ns <- ns + d12[i, "V2"],
                   di == "S", ns <- ns - d12[i, "V2"]))
  # print(c(ew,ns,di))
}

abs(ew) + abs(ns)



#---------
# part two
#---------

