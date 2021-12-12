
library(data.table)

#---------
# part one
#---------

# d <- scan("2021/10_data_test.txt", what = character())
d <- scan("2021/10_data.txt", what = character())

ref <- fread(text = "(:): 3 
[:]: 57 
{:}: 1197
<:>: 25137", sep = ":", col.names = c("sym_open", "sym_close", "points"))

sym_open <- ref$sym_open
sym_close <- ref$sym_close

# corrupted in example data: 3, 5, 6, 8, 9

dl <- strsplit(d, "")

expect_close <- \(x) {
  n <- length(x)
  
  closings <- c()
  
  incorrect <- FALSE
  
  for (i in 1:n) {
    if (x[i] %in% sym_open) {
      closings <- c(closings, sym_close[match(x[i], sym_open)])
    } else if (x[i] %in% sym_close & x[i] == tail(closings, 1)) {
      closings <- head(closings, -1)
    } else if (x[i] %in% sym_close & x[i] != tail(closings, 1)) {
      s <- x[i]
      incorrect <- TRUE
      break
    }
  }
  if (!incorrect) s <- NA
  s
}

res <- table(sapply(dl, expect_close))
sum(res * ref$points[match(names(res), sym_close)])


#---------
# part two
#---------

# d <- scan("2021/10_data_test.txt", what = character())
d <- scan("2021/10_data.txt", what = character())

ref <- fread(text = "(:): 3 
[:]: 57 
{:}: 1197
<:>: 25137", sep = ":", col.names = c("sym_open", "sym_close", "points"))

sym_open <- ref$sym_open
sym_close <- ref$sym_close

# incomplete in example data: 1, 2, 4, 7, 10

dl <- strsplit(d, "")

get_points <- \(x) {
  x <- rev(x)
  m <- c(0, match(x, sym_close))
  for (i in seq_along(x)) {
    if (i == 1) r <- m[i]
    r <- 5 * r + m[i + 1]
  }
  return(r)
}

to_complete <- \(x) {
  n <- length(x)
  
  closings <- c()
  
  incorrect <- FALSE
  
  for (i in 1:n) {
    if (x[i] %in% sym_open) {
      closings <- c(closings, sym_close[match(x[i], sym_open)])
    } else if (x[i] %in% sym_close & x[i] == tail(closings, 1)) {
      closings <- head(closings, -1)
    } else if (x[i] %in% sym_close & x[i] != tail(closings, 1)) {
      incorrect <- TRUE
    }
  }
  if (incorrect) closings <- NA
  closings
}

res <- sapply(lapply(dl, to_complete), get_points)
res <- sort(res[!is.na(res)])
res[(length(res) - 1)/2 + 1]




