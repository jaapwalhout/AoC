
#-------
# part 1
#-------

d <- readLines("2022/07_data.txt")

folder <- "/"
folder_list <- list()
dirs <- list()
counter <- 1L

for (r in d) {
  if (grepl("^\\$", r)) {
    run_cmd  <- TRUE
  }
  
  if (run_cmd) {
    if (grepl("\\$ cd", r)) {
      new_folder <- sub("\\$ cd ([a-z]+|/|\\.\\.)", "\\1", r)
      if (new_folder == "/") folder <- "/"
      else if (new_folder == "..") folder <- folder[-length(folder)]
      else folder <- c(folder, new_folder)
      folder_list[[counter]] <- folder
      counter <- counter + 1L
    }
    else if (grepl("\\$ ls", r)) {
      dirs[[folder]] <- list()
      run_cmd = FALSE
    }
  }
  else if (grepl("(\\d+).*", r)) {
    tmp_size <- sub("(\\d+).*", "\\1", r) |> as.integer()
    dirs[[folder]][["size"]] <- c(dirs[[folder]][["size"]], tmp_size)
  }
  else next
}

folder_list <- folder_list |> unique()

dir_sizes <- sapply(folder_list, \(x) unlist(dirs[[x]]) |> sum() )

dir_sizes[dir_sizes < 100000] |> sum()

#-------
# part 2
#-------

unused <- 70000000 - max(dir_sizes)

dir_sizes[dir_sizes > (30000000 - unused)] |> sort() |> head(1)

