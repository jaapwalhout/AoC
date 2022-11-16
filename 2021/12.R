
library(data.table)
library(igraph)

#---------
# part one
#---------

# d <- fread("2021/12_data_test.txt", sep = "-", header = FALSE)
# d <- fread("2021/12_data_test2.txt", sep = "-", header = FALSE)
d <- fread("2021/12_data.txt", sep = "-", header = FALSE)

g <- graph_from_data_frame(d, directed = FALSE)
plot.igraph(g)

n_edges <- rowSums(as_adjacency_matrix(g, sparse = FALSE))
l <- max(n_edges[grepl(pattern = "[A-Z]+", names(n_edges))])

path_list <- vector(mode = "list", length = l)
path_list[[1]] <- d

v1 <- grepl(pattern = "[A-Z]+", d$V1)
v2 <- grepl(pattern = "[A-Z]+", d$V2)
ix <- v1|v2

for (i in 2:l) {
  tmp <- d[ix]
  tmp[v1[ix], V1 := paste0(V1, i)]
  tmp[v2[ix], V2 := paste0(V2, i)]
  path_list[[i]] <- tmp
}

d2 <- rbindlist(path_list)

g2 <- graph_from_data_frame(d2, directed = FALSE)
plot.igraph(g2)

asp <- all_simple_paths(g2, from = "start", to = "end")
asp <- lapply(asp, names)
asp <- lapply(asp, sub, pattern = "([A-Za-z]+)\\d", replacement = "\\1")
asp <- unique(asp)

length(asp)


#---------
# part two
#---------

d <- fread("2021/12_data_test.txt", sep = "-", header = FALSE)
# d <- fread("2021/12_data_test2.txt", sep = "-", header = FALSE)
# d <- fread("2021/12_data.txt", sep = "-", header = FALSE)

paths <- rbindlist(list(d[V1 == "start", 1:2],
                        d[V2 == "start", 2:1]),
                   use.names = FALSE)

path_options <- rbindlist(list(d[!(V1 == "start" | V2 == "start")],
                               d[!(V1 == "start" | V2 == "start") & V2 != "end", 2:1]),
                          use.names = FALSE)

to_add <- setNames(path_options, paste0("V", ncol(paths) + (0:1)))

p2 <- merge(paths, to_add, by = names(paths)[2])
matrixStats::rowTabulates(as.matrix(p2))

alternatives <- rbind(d, setNames(d, names(d)[2:1])) |>
  (\(x) x[!(x$V1 %in% c('end', 'start')) & x$V2 != 'start',])()

small_caves <- (\(x) x[nchar(x) < 3 & tolower(x) == x])(unique(alternatives$V1))





#-----------------------------------------------------------------
# Solution below is with the igraph-package
# It works on the example datasets, but crashes on the actual puzzle input
# So: use with caution !!
#-----------------------------------------------------------------

g <- graph_from_data_frame(d, directed = FALSE)

n_edges <- rowSums(as_adjacency_matrix(g, sparse = FALSE))
iu <- grepl(pattern = "[A-Z]+", names(n_edges))
il <- grepl(pattern = "[a-z]+", names(n_edges)) &!(names(n_edges) %in% c("start","end"))

l_up <- max(n_edges[iu])
low_names <- names(n_edges)[il]

paths_up <- vector(mode = "list", length = l_up)
paths_up[[1]] <- d

v1 <- grepl(pattern = "[A-Z]+", d$V1)
v2 <- grepl(pattern = "[A-Z]+", d$V2)
ix <- v1|v2

for (i in 2:l_up) {
  tmp <- d[ix]
  tmp[v1[ix], V1 := paste0(V1, i)]
  tmp[v2[ix], V2 := paste0(V2, i)]
  paths_up[[i]] <- tmp
}

d2 <- rbindlist(paths_up)

paths_low <- vector(mode = "list", length = length(low_names))

for(li in seq_along(low_names)) {
  tmp_list <- list(d2)
  tmp <- d2[V1 == low_names[li] | V2 == low_names[li]]
  tmp[V1 == low_names[li], V1 := paste0(V1, 2)]
  tmp[V2 == low_names[li], V2 := paste0(V2, 2)]
  tmp_list[[2]] <- tmp
  paths_low[[li]] <- graph_from_data_frame(rbindlist(tmp_list), directed = FALSE)
}

paths <- vector(mode = "list", length = length(low_names))

for(pl in seq_along(paths_low)) {
  print(sprintf("Step %s - %s", pl, Sys.time()))
  asp <- all_simple_paths(paths_low[[pl]], from = "start", to = "end")
  print(sprintf("  - paths created - %s", Sys.time()))
  asp <- lapply(asp, names)
  print(sprintf("  - names extracted - %s", Sys.time()))
  asp <- lapply(asp, sub, pattern = "([A-Za-z]+)\\d", replacement = "\\1")
  print(sprintf("  - digits removed from names - %s", Sys.time()))
  paths[[pl]] <- unique(asp)
  print(sprintf("  - unique names added to list - %s", Sys.time()))
  rm(asp)
  gc()
}

res <- Reduce(union, paths)
length(res)

