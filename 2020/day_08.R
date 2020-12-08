
#---------
# part one
#---------

# d8 <- read.table(text="nop +0
# acc +1
# jmp +4
# acc +3
# jmp -3
# acc -99
# acc +1
# jmp -4
# acc +6", header=TRUE, stringsAsFactors=FALSE)

d8 <- read.table("08_data.txt")

f <- function(data) {
  to_visit <- 1
  visited <- integer()
  accumulator <- 0

  while( !to_visit %in% visited & to_visit %in% 1:nrow(data) ) {
    tmp <- data[to_visit,]
    if (tmp[[1]] == "acc") {
      accumulator <- accumulator + tmp[[2]]
      visited <- c(visited, to_visit)
      to_visit <- to_visit + 1
    } else if (tmp[[1]] == "jmp") {
      visited <- c(visited, to_visit)
      to_visit <- to_visit + tmp[[2]]
    } else {
      visited <- c(visited, to_visit)
      to_visit <- to_visit + 1
    }
  }
  rm(tmp)
  return(accumulator)
}

f(d8)

#---------
# part two
#---------

d8 <- read.table("08_data.txt")

f <- function(data) {
  to_visit <- 1
  visited <- integer()
  accumulator <- 0

  while( !to_visit %in% visited & to_visit %in% 1:nrow(data) ) {
    tmp <- data[to_visit,]
    if (tmp[[1]] == "acc") {
      accumulator <- accumulator + tmp[[2]]
      visited <- c(visited, to_visit)
      to_visit <- to_visit + 1
    } else if (tmp[[1]] == "jmp") {
      visited <- c(visited, to_visit)
      to_visit <- to_visit + tmp[[2]]
    } else {
      visited <- c(visited, to_visit)
      to_visit <- to_visit + 1
    }
  }
  rm(tmp)
  return(list(accumulator, to_visit))
}

for (i in which(d8[[1]] %in% c("jmp","nop"))) {
  d <- d8
  d[i, 1] <- c("jmp","nop")[1L + (d[i, 1] == "jmp")]
  l <- f(d)
  if ( l[[2]] > nrow(d) ) {
    print(l[[1]])
    break
  }
}

