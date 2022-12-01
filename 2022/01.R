

#-------
# part 1
#-------

d01 <- scan("2022/01_data.txt", what = integer(), blank.lines.skip = FALSE)

tapply(d01, cumsum(is.na(d01)), FUN = sum, na.rm = TRUE) |> max(na.rm = TRUE)

# d1 <- data.frame(g = cumsum(is.na(d01)), s = d01)
# d1 <- d1[!is.na(d1$s),]
# ave(d1$s, d1$g, FUN = cumsum) |> max()

#-------
# part 2
#-------

tapply(d01, cumsum(is.na(d01)), FUN = sum, na.rm = TRUE) |> 
  sort() |> tail(3) |> sum()

# tapply(d1$s, d1$g, FUN = sum) |> sort() |> tail(3) |> sum()
