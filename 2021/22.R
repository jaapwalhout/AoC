
library(data.table)

#---------
# part one
#---------

reactor <- CJ(x = -50:50, y = -50:50, z = -50:50)[, state := "off"]

steps <- scan("2021/22_data.txt", what = character(), sep = "~")

parse_steps <- \(s) {
  tmp <- strsplit(s, " ")
  
  for (j in seq_along(tmp)) {
    cube_state <- tmp[[j]][1]
    
    xyz <- strsplit(tmp[[j]][2], ",")[[1]] |>
      sub(pattern = "\\.{2}", replacement = ":") |>
      strsplit(split = "=")
    
    l <- vector(mode = "list", length = 3L)
    for (i in seq_along(l)) {
      l[[i]] <- eval(parse(text = xyz[[i]][2]))
    }
    names(l) <- sapply(xyz, `[[`, 1)
    
    l <- lapply(l, \(x) {
      x[inrange(x,-50,50)]
    })
    
    reboot_step <- do.call(CJ, l)[, state := cube_state]
    
    reactor[reboot_step, on = .(x, y, z), state := i.state]
  }
}

parse_steps(steps)

reactor[, sum(state == "on")]

parse_step <- \(s) {
  tmp <- strsplit(s, " ")[[1]]
  cube_state <- tmp[1]
  
  xyz <- strsplit(tmp[2], ",")[[1]] |>
    sub(pattern = "\\.{2}", replacement = ":") |>
    strsplit(split = "=")
  
  l <- vector(mode = "list", length = 3L)
  for (i in seq_along(l)) {
    l[[i]] <- eval(parse(text = xyz[[i]][2]))
  }
  names(l) <- sapply(xyz, `[[`, 1)
  
  do.call(CJ, l)[, state := cube_state]
}

tmp <- parse_step(steps[1])

do.call(expand.grid, tmp$xyx)



# eval(parse(text = parse_step(steps[1])$xyz[1]))
