
#---------
# part one
#---------

d <- "2021/20_data_test.txt"
d <- "2021/20_data.txt"

algo <- scan(d, what = character(), n = 1) |>
  chartr(old = ".#", new = "01") |>
  strsplit("") |>
  unlist() |>
  as.integer()
img <- scan(d, what = character(), skip = 2) |>
  chartr(old = ".#", new = "01") |>
  strsplit("") |>
  lapply(as.integer) |>
  do.call(what = rbind)

grow_image <- \(m, n = 2, fill = 0) {
  m.0 <- matrix(fill, nrow = nrow(m) + (n * 2), ncol = ncol(m) + (n * 2))
  m.1 <- m.0
  m.0[row(m)[,1] + n, col(m)[1,] + n] <- m
  
  ri <- 2:(nrow(m.0) - 1)
  ci <- 2:(ncol(m.0) - 1)
  
  for ( i in ri ) {
    for ( j in ci ) {
      v <- m.0[(i-1):(i+1),(j-1):(j+1)] |> t() |> c()
      m.1[i,j] <- algo[sum(v * (2^(8:0))) + 1]
    }
  }
  return(m.1[ri, ci])
}

tmp_img <- img
for (i in 1:2) {
  tmp_img <- grow_image(tmp_img, fill = tmp_img[1,1])
}
sum(tmp_img)

#---------
# part two
#---------

tmp_img <- img
for (i in 1:50) {
  tmp_img <- grow_image(tmp_img, fill = tmp_img[1,1])
}
sum(tmp_img)
