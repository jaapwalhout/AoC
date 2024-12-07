
library(data.table)

# d <- scan(file = "05_data_test.txt", what = character()) |> as.integer()
d <- scan(file = "05_data.txt", what = character()) |> as.numeric()
d <- split(d[!is.na(d)], cumsum(is.na(d))[!is.na(d)])

seeds <- d[[1]]
seed2soil <- matrix(d[[2]], ncol = 3, byrow = TRUE)
soil2fert <- matrix(d[[3]], ncol = 3, byrow = TRUE)
fert2water <- matrix(d[[4]], ncol = 3, byrow = TRUE)
water2light <- matrix(d[[5]], ncol = 3, byrow = TRUE)
light2temp <- matrix(d[[6]], ncol = 3, byrow = TRUE)
temp2hum <- matrix(d[[7]], ncol = 3, byrow = TRUE)
hum2loc <- matrix(d[[8]], ncol = 3, byrow = TRUE)


#-------
# part 1
#-------

get_dest <- \(x, m) {
  w <- sapply(
    x,
    between,
    lower = m[,2],
    upper = m[,2] + m[,3] - 1L
  ) |>
    which(arr.ind = TRUE)
  x[w[,2]] <- m[w[,1],1] + x[w[,2]] - m[w[,1],2]
  x
}

get_dest(seeds, seed2soil) |>
  get_dest(soil2fert) |>
  get_dest(fert2water) |>
  get_dest(water2light) |>
  get_dest(light2temp) |>
  get_dest(temp2hum) |>
  get_dest(hum2loc) |>
  min()

#-------
# part 2
#-------

seed2soil <- matrix(d[[2]], ncol = 3, byrow = TRUE) |> as.data.table()
soil2fert <- matrix(d[[3]], ncol = 3, byrow = TRUE) |> as.data.table()
fert2water <- matrix(d[[4]], ncol = 3, byrow = TRUE) |> as.data.table()
water2light <- matrix(d[[5]], ncol = 3, byrow = TRUE) |> as.data.table()
light2temp <- matrix(d[[6]], ncol = 3, byrow = TRUE) |> as.data.table()
temp2hum <- matrix(d[[7]], ncol = 3, byrow = TRUE) |> as.data.table()
hum2loc <- matrix(d[[8]], ncol = 3, byrow = TRUE) |> as.data.table()

seed2soil[, V4 := V2 + V3 -1]
soil2fert[, V4 := V2 + V3 -1]
fert2water[, V4 := V2 + V3 -1]
water2light[, V4 := V2 + V3 -1]
light2temp[, V4 := V2 + V3 -1]
temp2hum[, V4 := V2 + V3 -1]
hum2loc[, V4 := V2 + V3 -1]

get_dest_DT <- \(x, m) {
  seedsx <- data.table(s = x, s1 = x)
  seedsx[m
         , on = .(s >= V2, s <= V4)
         , d := s1 + V1 - V2]
  seedsx[is.na(d), d := s]
  seedsx$d
}

x <- seeds[c(TRUE, FALSE)]
y <- seeds[c(FALSE, TRUE)] + seeds[c(TRUE, FALSE)] - 1

minima <- vector(mode = "numeric", length = length(x))

for (r in 1:length(x)) {
  seeds2 <- x[r]:y[r]

  minima[r] <- get_dest_DT(seeds2, seed2soil) |>
    get_dest_DT(soil2fert) |>
    get_dest_DT(fert2water) |>
    get_dest_DT(water2light) |>
    get_dest_DT(light2temp) |>
    get_dest_DT(temp2hum) |>
    get_dest_DT(hum2loc) |>
    min()

  rm(seeds2)
  invisible(gc())
  print(r)
}

minima |> min()
minima |> sort()

