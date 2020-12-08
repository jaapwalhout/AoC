
#---------
# part one
#---------

passes <- strsplit(scan(file = "05_data.txt", what = character()), "")

rows <- 0:127
seats <- 0:7

f <- function(p) {
  # which row
  for (i in 1:7) {
    if(i == 1) r <- rows
    if(p[i] == "F") {
      r <- head(r, (128/(2^i)))
    } else {
      r <- tail(r, 128/(2^i))
    }
  }
  # which seat
  for (i in 1:3) {
    if(i == 1L) s <- seats
    if(p[i + 7L] == "L") {
      s <- head(s, (8/(2^i)))
    } else {
      s <- tail(s, 8/(2^i))
    }
  }
  return((r * 8) + s)
}

seat_ids <- sapply(passes, f)
max(seat_ids)

#---------
# part two
#---------

setdiff(min(seat_ids):max(seat_ids), seat_ids)

