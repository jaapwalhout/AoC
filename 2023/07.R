
library(data.table)

# d <- fread(file = "2023/07_data_test.txt")
d <- fread(file = "2023/07_data.txt")

cards1 <- c(2:9, "T", "J", "Q", "K", "A")

hands1 <- strsplit(d$V1, "")

hand_types1 <- hands1 |>
  lapply(table) |>
  lapply(sort, decreasing = TRUE)
hand_types1 <- setNames(sapply(hand_types1, \(x) paste0(x, collapse = "")),
                        sapply(hand_types1, \(x) paste0(names(x), collapse = "")))

types <- c("11111","2111","221","311","32","41","5")

equal_val_order <- \(x, hh, cc) {
  h <- hh[x]
  if (length(h) == 1) {
    r <- 1
  } else {
    r <- sapply(h, \(x) {
      match(x, cc) |>
        sprintf(fmt = "%02d") |>
        paste0(collapse = "") |>
        as.numeric()
    }) |> frank()
  }
  r
}


#-------
# part 1
#-------

m1 <- match(hand_types1, types)

ix <- data.table(m = m1)[, r := .I
                         ][, o := equal_val_order(r, hh = hands1, cc = cards1)
                           , by = m
                           ][, .(m, o)] |>
  frank()

{ix * d$V2} |> sum()


#-------
# part 2
#-------

cards2 <- c("J", 2:9, "T", "Q", "K", "A")

hands2 <- lapply(hands1, \(x) {
  if (any(x == "J") & !all(x == "J")) {
    tx <- table(x)
    w <- which(names(tx) != "J")
    tx <- tx[w]
    nms <- names(tx)[tx == max(tx)]
    x[x == "J"] <- nms[which.max(match(nms, cards2))]
  }
  x
})

hand_types2 <- hands2 |>
  lapply(table) |>
  lapply(sort, decreasing = TRUE)
hand_types2 <- setNames(sapply(hand_types2, \(x) paste0(x, collapse = "")),
                        sapply(hand_types2, \(x) paste0(names(x), collapse = "")))

m2 <- match(hand_types2, types)

ix <- data.table(m = m2)[, r := .I
                         ][, o := equal_val_order(r, hh = hands1, cc = cards2)
                           , by = m
                           ][, .(m, o)] |>
  frank()

{ix * d$V2} |> sum()
