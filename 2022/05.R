
#-------
# part 1
#-------

d <- scan("2022/05_data.txt", what = character(), blank.lines.skip = FALSE, sep = "~")
i <- which(d == "")

g1 <- gregexpr("\\d", d[i - 1])

s <- do.call(rbind, sapply(d[1:(i - 2)], regmatches, m = g1, USE.NAMES = FALSE)) |> t()
crates <- lapply(split(s, 1:nrow(s)), \(x) setdiff(x, " "))

m <- d[(i + 1):length(d)]
m <- regmatches(m, gregexpr("\\d+", m))
moves <- do.call(rbind, m) |> type.convert()

for (i in seq_len(nrow(moves))) {
  crates[[moves[i,3]]] <- c(crates[[moves[i,2]]][rev(seq_len(moves[i,1]))], crates[[moves[i,3]]])
  crates[[moves[i,2]]] <- crates[[moves[i,2]]][-seq_len(moves[i,1])]
}

sapply(crates, `[`, 1) |> paste0(collapse = "")

#-------
# part 2
#-------

for (i in seq_len(nrow(moves))) {
  crates[[moves[i,3]]] <- c(crates[[moves[i,2]]][seq_len(moves[i,1])], crates[[moves[i,3]]])
  crates[[moves[i,2]]] <- crates[[moves[i,2]]][-seq_len(moves[i,1])]
}

sapply(crates, `[`, 1) |> paste0(collapse = "")
