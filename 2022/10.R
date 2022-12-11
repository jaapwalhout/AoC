
library(data.table)

#-------
# part 1
#-------

d <- scan("2022/10_data.txt", what = character()) |> as.integer()
d[is.na(d)] <- 0L

i1 <- seq(20, 220, 40)

{cumsum(c(1L, d))[i1] * i1} |> sum()


#-------
# part 2
#-------

s <- cumsum(c(1L, d)) + 1L

i2 <- mapply(\(x, y) (x %% 40) %in% y,
             x = 1:240,
             y = Map(`:`, s - 1, s + 1) |> head(-1))

c(" ","#")[i2 + 1L] |> 
  matrix(nrow = 6, ncol = 40, byrow = TRUE) |>
  apply(1, paste0, collapse = "") |>
  as.list() |>
  do.call(what = rbind)
