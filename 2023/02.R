
library(stringi)
library(matrixStats)

kleuren <- c("red", "green", "blue")
maximums <- 12:14

# games <- readLines("2023/02_data_test.txt")
games <- readLines("2023/02_data.txt")

nums <- sub(pattern = "Game ([0-9]+)\\:.*", "\\1", games) |> as.integer()

games <- sub(pattern = "^.*\\:\\s(.*)", "\\1", games) |>
  strsplit(split = ";") |>
  lapply(\(x) trimws(x)) |>
  lapply(\(x) {
    lapply(x, \(y) {
      s <- stri_split_regex(y, pattern = " |, ") |> stri_list2matrix()
      setNames(as.integer(s[c(TRUE,FALSE)]), s[c(FALSE,TRUE)])
    })
  })

#-------
# part 1
#-------

w <- sapply(games, \(x) {
  sapply(x, \(y) {
    m <- match(names(y), kleuren)
    {maximums[m] < y} |> any()
  }) |> any()
})

nums[!w] |> sum()


#-------
# part 2
#-------

sapply(games, \(x) {
  l <- lapply(x, stack)
  r <- Reduce(\(...) merge(..., by = "ind", all = TRUE), l)
  rowMaxs(as.matrix(r[,-1]), na.rm = TRUE) |> prod()
}) |> sum()
