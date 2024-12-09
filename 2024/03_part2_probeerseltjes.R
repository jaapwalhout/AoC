g2 <- Map(
  \(x, y, z) {
    i <- x < y | x > z
    ml <- attr(x, "match.length")[i]
    x <- x[i]
    attr(x, "match.length") <- ml
    attr(x,"index.type") <- "chars"
    attr(x,"useBytes") <- TRUE
    return(x)
  },
  x = g,
  y = g_no,
  z = g_do
)
r <- regmatches(d, g2) |> unlist()

sub(pattern = "mul", replacement = "prod", r, fixed = TRUE) |> 
  sapply(\(x) eval(parse(text = x))) |>
  sum()


Map(
  \(x, y, z) {
    list(x, y, z)
  },
  x = g[13],
  y = g_no[13],
  z = g_do[13]
)

733 > g_no[[13]]
733 < g_do[[13]]

Map(
  \(x, y, z) {
    while (length(y) > 0 & length(z) > 0) {
      for (n in seq_along(x)) {
        no <- x[n] < y[1]
        do <- x[n] > z[1]
        if(!no) y <- head(y, -1)
        if(do) z <- head(z, -1)
      }
    }
  },
  x = g[13],
  y = g_no[13],
  z = g_do[13]
)

d1 <- sapply(g_no[[13]], \(x) g[[13]] < x) |> rowSums() |> diff()
d2 <- sapply(g_do[[13]], \(x) g[[13]] > x) |> rowSums() |> diff()

abs(d1) > d2

s1 <- sapply(g_no[[13]], \(x) g[[13]] < x) |> rowSums() - 4
s2 <- sapply(g_do[[13]][2], \(x) g[[13]] > x) |> rowSums() 

s <- {s1 + s2} |> diff()
s <- c(0, s)

which(g_do[[13]] > g_no[[13]][1])

sapply(g_no, \(x) x[1] == -1)
sapply(g_do, \(x) x[1] == -1)

k <- 12

Map(
  \(x, y, z) {
    if (y[1] != -1 & z[1] != -1) {
      i <- which(z > y[1])
      z <- z[i]
      
      s0 <- as.integer(y[1] > x[1]) - 1
      s1 <- sapply(y, \(v) x < v) |> rowSums() - 4
      if (length(z) == 0) {
        s2 <- 0
      } else {
        s2 <- sapply(z, \(v) x > v) |> rowSums()
      }
      
      s <- {s1 + s2} |> diff()
      rs <- c(s0, s) |> rle()
      rv <- rs$values + c(0, rs$values |> head(-1))
      r <- rep(rv, rs$lengths)
      
      ix <- r > -1
      ml <- attr(x, "match.length")[ix]
      x <- x[ix]
      attr(x, "match.length") <- ml
      attr(x,"index.type") <- "chars"
      attr(x,"useBytes") <- TRUE
    } else if (y[1] != -1 & z[1] == -1) {
      ix <- x < y[1]
      ml <- attr(x, "match.length")[ix]
      x <- x[ix]
      attr(x, "match.length") <- ml
      attr(x,"index.type") <- "chars"
      attr(x,"useBytes") <- TRUE
    } else {
      x
    }
    
    if (length(x) == 0) {
      x <- -1
      attr(x, "match.length") <- -1
      attr(x,"index.type") <- "chars"
      attr(x,"useBytes") <- TRUE
    }
    return(x)
  },
  x = g[k],
  y = g_no[k],
  z = g_do[k]
)
g[k]
g_no[k]
g_do[k]

for (k in seq_along(g)) {
  print(k)
  g3 <- Map(
    \(x, y, z) {
      if (y[1] != -1 & z[1] != -1) {
        i <- which(z > y[1])
        z <- z[i]
        
        s0 <- as.integer(y[1] > x[1]) - 1
        s1 <- sapply(y, \(v) x < v) |> rowSums() - 4
        if (length(z) == 0) {
          s2 <- 0
        } else {
          s2 <- sapply(z, \(v) x > v) |> rowSums()
        }
        
        s <- {s1 + s2} |> diff()
        rs <- c(s0, s) |> rle()
        rv <- rs$values + c(0, rs$values |> head(-1))
        r <- rep(rv, rs$lengths)
        
        ix <- r > -1
        ml <- attr(x, "match.length")[ix]
        x <- x[ix]
        attr(x, "match.length") <- ml
        attr(x,"index.type") <- "chars"
        attr(x,"useBytes") <- TRUE
      } else if (y[1] != -1 & z[1] == -1) {
        ix <- x < y[1]
        ml <- attr(x, "match.length")[ix]
        x <- x[ix]
        attr(x, "match.length") <- ml
        attr(x,"index.type") <- "chars"
        attr(x,"useBytes") <- TRUE
      } else {
        x
      }
      if (length(x) == 0) {
        x <- -1
        attr(x, "match.length") <- -1
        attr(x,"index.type") <- "chars"
        attr(x,"useBytes") <- TRUE
      }
      return(x)
    },
    x = g[k],
    y = g_no[k],
    z = g_do[k]
  )
  print(g3)
  print(g[k])
  print(g_no[k])
  print(g_do[k])
  ans <- readline("Volgende?")
  if (ans == "j") next else break
}
