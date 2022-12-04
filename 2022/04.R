
library(data.table)

#-------
# part 1
#-------

d <- fread("2022/04_data.txt", header = FALSE)

f1 <- \(x, y) {
  x <- strsplit(x, "-")[[1]] |> type.convert()
  y <- strsplit(y, "-")[[1]] |> type.convert()
  
  all(x[1]:x[2] %in% y[1]:y[2]) | all(y[1]:y[2] %in% x[1]:x[2])
}

mapply(f1, d$V1, d$V2) |> sum()


#-------
# part 2
#-------

d <- fread("2022/04_data.txt", header = FALSE)

f2 <- \(x, y) {
  x <- strsplit(x, "-")[[1]] |> type.convert()
  y <- strsplit(y, "-")[[1]] |> type.convert()
  
  any(x[1]:x[2] %in% y[1]:y[2]) | any(y[1]:y[2] %in% x[1]:x[2])
}

mapply(f2, d$V1, d$V2) |> sum()

