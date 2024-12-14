
options(scipen = 999)
library(data.table)

# ------
# part 1
# ------

# d <- readLines("2024/14_data_test.txt")
d <- readLines("2024/14_data.txt")

robots <- strsplit(d, " ") |>
  lapply(substring, first = 3) |>
  lapply(\(x) strsplit(x, split = ",") |> unlist() |> as.numeric() ) |>
  do.call(what = rbind) |>
  as.data.frame()
names(robots) <- paste0(rep(c("p","v"), each = 2), rep(c("x","y"), 2))

# dims <- c(11,7)
dims <- c(101,103)

tmp <- 1 + robots[, c("px","py")] + robots[, c("vx","vy")] * 100
tmp[[1]] <- tmp[[1]] %% dims[1] #+ 1L
tmp[[2]] <- tmp[[2]] %% dims[2] #+ 1L
tmp <- aggregate(1:nrow(tmp), tmp, length)

tmp[, c("px","py")] <- tmp[, c("px","py")] + sweep((tmp[,1:2] == 0), 2, dims, "*")

# ix <- tmp$px != ceiling(dims[1]/2)
# iy <- tmp$py != ceiling(dims[2]/2)
# tmp[ix & iy,]$x |> sum()

sx <- ceiling(dims[1]/2)
sy <- ceiling(dims[2]/2)
q1 <- tmp[tmp$px < sx & tmp$py < sy,]$x |> sum()
q2 <- tmp[tmp$px > sx & tmp$py < sy,]$x |> sum()
q3 <- tmp[tmp$px < sx & tmp$py > sy,]$x |> sum()
q4 <- tmp[tmp$px > sx & tmp$py > sy,]$x |> sum()

prod(q1, q2, q3, q4)
