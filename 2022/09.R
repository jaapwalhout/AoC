
#-------
# part 1
#-------

d <- scan("2022/09_data.txt", what = character())
dirs <- d[c(TRUE, FALSE)]
steps <- d[c(FALSE, TRUE)] |> type.convert()

pos_list <- list()
pos_list[[1]] <- list(c(0,0))

pos_h <- c(0,0)
pos_t <- c(0,0)

for (i in seq_along(dirs)) {
  lh <- list()
  lt <- list()
  
  if (dirs[i] == "R") {
    for (j in seq(steps[i])) {
      lh[[j]] <- pos_h + c(j,0)
      lt[[j]] <- pos_h + c((j-1),0)
    }
    pos_list[[i + 1]] <- lt[sapply(lh, \(x) max(abs(x - pos_t)) > 1)]
    pos_h <- lh[[j]]
    pos_t <- if (length(pos_list[[i + 1]]) > 0) lt[[j]] else pos_t
  }
  else if (dirs[i] == "L") {
    for (j in seq(steps[i])) {
      lh[[j]] <- pos_h + c(-j,0)
      lt[[j]] <- pos_h + c(-(j-1),0)
    }
    pos_list[[i + 1]] <- lt[sapply(lh, \(x) max(abs(x - pos_t)) > 1)]
    pos_h <- lh[[j]]
    pos_t <- if (length(pos_list[[i + 1]]) > 0) lt[[j]] else pos_t
  }
  else if (dirs[i] == "U") {
    for (j in seq(steps[i])) {
      lh[[j]] <- pos_h + c(0,j)
      lt[[j]] <- pos_h + c(0,(j-1))
    }
    pos_list[[i + 1]] <- lt[sapply(lh, \(x) max(abs(x - pos_t)) > 1)]
    pos_h <- lh[[j]]
    pos_t <- if (length(pos_list[[i + 1]]) > 0) lt[[j]] else pos_t
  }
  else if (dirs[i] == "D") {
    for (j in seq(steps[i])) {
      lh[[j]] <- pos_h + c(0,-j)
      lt[[j]] <- pos_h + c(0,-(j-1))
    }
    pos_list[[i + 1]] <- lt[sapply(lh, \(x) max(abs(x - pos_t)) > 1)]
    pos_h <- lh[[j]]
    pos_t <- if (length(pos_list[[i + 1]]) > 0) lt[[j]] else pos_t
  }
}

unlist(pos_list, recursive = FALSE) |> unique() |> length()

