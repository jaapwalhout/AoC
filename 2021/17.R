
library(data.table)

#---------
# part one
#---------

xmin <- 20
xmax <- 30
ymin <- -10
ymax <- -5

xmin <- 138
xmax <- 184
ymin <- -125
ymax <- -71


get_path_max <- \(x, y) {
  x1 <- xp <- x
  y1 <- yp <- y
  
  repeat {
    x <- max(0, (x - 1))
    y <- y - 1
    
    xpos <- x + xp[1]
    ypos <- y + yp[1]
    
    xp <- c(xpos, xp)
    yp <- c(ypos, yp)
    
    if (xpos > xmax | ypos < ymin) break
  }
  
  check <- any(between(xp, xmin, xmax) + between(yp, ymin, ymax) == 2)
  max_y <- max(yp) * NA^(!check)
  
  return(max_y)
}

m <- 0

for (x in 1:100) {
  for (y in -125:300) {
    m <- max(m, get_path_max(x, y), na.rm = TRUE)
  }
}

m


#---------
# part two
#---------

xmin <- 20
xmax <- 30
ymin <- -10
ymax <- -5

xmin <- 138
xmax <- 184
ymin <- -125
ymax <- -71


in_target <- \(x, y) {
  x1 <- xp <- x
  y1 <- yp <- y
  
  repeat {
    x <- max(0, (x - 1))
    y <- y - 1
    
    xpos <- x + xp[1]
    ypos <- y + yp[1]
    
    xp <- c(xpos, xp)
    yp <- c(ypos, yp)
    
    if (xpos > xmax | ypos < ymin) break
  }
  
  check <- any(between(xp, xmin, xmax) + between(yp, ymin, ymax) == 2)
  
  return(check)
}

r <- 0

for (x in 1:185) {
  for (y in -126:500) {
    r <- r + in_target(x, y)
  }
}
r


