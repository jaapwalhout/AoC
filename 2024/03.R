
# ------
# part 1
# ------

# test data
mul <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

g <- gregexpr(pattern = "mul\\([0-9]+,[0-9]+\\)", text = mul)
r <- regmatches(mul, g)[[1]]

sub(pattern = "mul", replacement = "prod", r, fixed = TRUE) |> 
  sapply(\(x) eval(parse(text = x))) |>
  sum()

# puzzle data
d <- scan(file = "2024/03_data.txt", what = character())

g <- gregexpr(pattern = "mul\\([0-9]+,[0-9]+\\)", text = d)
r <- regmatches(d, g) |> unlist()

sub(pattern = "mul", replacement = "prod", r, fixed = TRUE) |> 
  sapply(\(x) eval(parse(text = x))) |>
  sum()


# ------
# part 2
# ------

# test data
mul <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

g <- gregexpr(pattern = "mul\\([0-9]+,[0-9]+\\)", text = mul)
g_no <- gregexpr(pattern = "don't\\(\\)", text = mul)
g_do <- gregexpr(pattern = "do\\(\\)", text = mul)

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
r <- regmatches(mul, g2)[[1]]

sub(pattern = "mul", replacement = "prod", r, fixed = TRUE) |> 
  sapply(\(x) eval(parse(text = x))) |>
  sum()

# puzzle data
d <- scan(file = "2024/03_data.txt", what = character())

g <- gregexpr(pattern = "mul\\([0-9]+,[0-9]+\\)", text = d)
g_no <- gregexpr(pattern = "don't\\(\\)", text = d)
g_do <- gregexpr(pattern = "do\\(\\)", text = d)

g2 <- Map(
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
  x = g,
  y = g_no,
  z = g_do
)

r <- regmatches(d, g2) |> unlist()

sub(pattern = "mul", replacement = "prod", r, fixed = TRUE) |> 
  sapply(\(x) eval(parse(text = x))) |>
  sum()

fout1 <- 133519632
