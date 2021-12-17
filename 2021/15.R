
library(data.table)
library(igraph)

#---------
# part one
#---------

# d <- scan("2021/15_data_test.txt", what = character())
d <- scan("2021/15_data.txt", what = character())

d <- tstrsplit(d, split = "", type.convert = TRUE)
d <- as.matrix(setDT(d))

nr <- nrow(d)
nc <- ncol(d)

dt <- data.table(ii = rep(1:nr, nc),
                 jj = rep(1:nr, each = nc))
dt[, r := d[cbind(ii,jj)]]

nodes <- dt[, .(ii2 = .BY[[1]] + c(1,-1,0,0),
                jj2 = .BY[[2]] + c(0,0,1,-1))
            , by = .(ii, jj, r)
            ][between(ii2, 1, nr) & between(jj2, 1, nr)
              ][, `:=` (node1 = paste(ii, jj, sep=","),
                        node2 = paste(ii2, jj2, sep=","))
                ][, .(ii2, jj2, node1, node2)
                  ][dt, on = .(ii2 = ii, jj2 = jj), weight := r
                    ][, .(node1, node2, weight)]

g <- graph_from_data_frame(nodes, directed = TRUE)

paths <- shortest_paths(g, from = "1,1", to = paste0(nr, ",", nc))

nms <- names(paths$vpath[[1]])

nodes[.(head(nms, -1), tail(nms, -1))
      , on = .(node1, node2)
      , sum(weight)]


#---------
# part two
#---------

# d <- scan("2021/15_data_test.txt", what = character())
d <- scan("2021/15_data.txt", what = character())

d <- tstrsplit(d, split = "", type.convert = TRUE)
d <- as.matrix(setDT(d))

d <- do.call(cbind, lapply(0:4, \(x) d + x))
d <- do.call(rbind, lapply(0:4, \(x) d + x))

d[d > 9] <- d[d > 9] - 9L

nr <- nrow(d)
nc <- ncol(d)

dt <- data.table(ii = rep(1:nr, nc),
                 jj = rep(1:nr, each = nc))
dt[, r := d[cbind(ii,jj)]]

nodes <- dt[, .(ii2 = .BY[[1]] + c(1,-1,0,0),
                jj2 = .BY[[2]] + c(0,0,1,-1))
            , by = .(ii, jj, r)
            ][between(ii2, 1, nr) & between(jj2, 1, nr)
              ][, `:=` (node1 = paste(ii, jj, sep=","),
                        node2 = paste(ii2, jj2, sep=","))
                ][, .(ii2, jj2, node1, node2)
                  ][dt, on = .(ii2 = ii, jj2 = jj), weight := r
                    ][, .(node1, node2, weight)]

g <- graph_from_data_frame(nodes, directed = TRUE)

paths <- shortest_paths(g, from = "1,1", to = paste0(nr, ",", nc))

nms <- names(paths$vpath[[1]])

nodes[.(head(nms, -1), tail(nms, -1))
      , on = .(node1, node2)
      , sum(weight)]

