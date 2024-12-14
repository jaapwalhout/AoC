
options(scipen = 99)

# ------
# part 1
# ------

# d <- scan("2024/05_data_test.txt", what = character(), sep = "\n")
d <- scan("2024/05_data.txt", what = character(), sep = "\n")

por <- d[grepl("|", d, fixed = TRUE)] |> 
  strsplit(split = "|", fixed = TRUE) |>
  lapply(as.integer) |>
  do.call(what = rbind) |>
  as.data.frame()
ptp <- d[!grepl("|", d, fixed = TRUE)] |> 
  strsplit(split = ",", fixed = TRUE) |>
  lapply(as.integer)

ix <- sapply(ptp, \(x) {
  xlog <- vector(length = length(x))
  for (i in seq_along(x)) {
    if (i == 1) {
      xlog[i] <- all(x[-1] %in% por[por$V1 == x[i],]$V2)
    } else if (i != length(x)) {
      xlog[i] <- all(
        head(x, i-1) %in% por[por$V2 == x[i],]$V1 &
          tail(x, -i) %in% por[por$V1 == x[i],]$V2
      )
    } else {
      xlog[i] <- TRUE
    }
  }
  all(xlog)
})

get_middle <- \(x) x[ceiling(length(x)/2)]

sapply(ptp[ix], get_middle) |> sum()
