
library(data.table)

# d <- scan(file = "2023/03_data_test.txt", what = character())
d <- scan(file = "2023/03_data.txt", what = character())

m <- strsplit(d, "") |> do.call(what = rbind)

g <- gregexpr(pattern = "\\d+", d)
rm <- regmatches(d, g)

#-------
# part 1
#-------

w_partnum <- matrix(grepl("\\d", m), ncol = ncol(m)) |>
  which(arr.ind = TRUE) |>
  as.data.table() |>
  setorder()
w_partnum[, part := cumsum(c(1, diff(col) > 1)), by = row]

w_symbols <- matrix(!grepl("\\d|\\.", m), ncol = ncol(m)) |>
  which(arr.ind = TRUE) |>
  as.data.table()

w_reach <- w_symbols |>
  split(f = 1:nrow(w_symbols)) |>
  lapply(\(x) {
    expand.grid(row = x[[1]] + (-1:1), col = x[[2]] + (-1:1))
  } ) |>
  do.call(what = rbind.data.frame) |>
  setDT() |>
  unique()

part_match <- merge(w_partnum, w_reach) |>
  setorder()

parts <- character()

for(r in unique(part_match$row)) {
  parts <- c(parts, rm[[r]][part_match[row == r, unique(part)]])
}

parts |> as.integer() |> sum()


#-------
# part 2
#-------

w_partnum <- matrix(grepl("\\d", m), ncol = ncol(m)) |>
  which(arr.ind = TRUE) |>
  as.data.table() |>
  setorder()
w_partnum[, part := cumsum(c(1, diff(col) > 1)), by = row]

w_gears <- {m == "*"} |>
  which(arr.ind = TRUE) |>
  as.data.table()
w_gears[, gear := .I]

w_reach <- w_gears |>
  split(f = 1:nrow(w_gears)) |>
  lapply(\(x) {
    expand.grid(row = x[[1]] + (-1:1), col = x[[2]] + (-1:1)) |> data.frame(gear = x[[3]])
  } ) |>
  do.call(what = rbind.data.frame) |>
  setDT() |>
  unique()

part_match <- merge(w_partnum, w_reach) |>
  setorder()

part_match <- part_match[, .SD[uniqueN(.SD, by = c("row", "part")) == 2], by = gear]

part_match[, .(gear_ratio = {
                  parts <- integer()
                  for(r in unique(row)) {
                    parts <- c(parts, rm[[r]][.SD[row == r, unique(part)]] |> as.integer())
                  }
                  prod(parts)
                }
               )
           , by = gear][, sum(gear_ratio)]

