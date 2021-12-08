
library(data.table)

#---------
# part one
#---------

# d <- fread("2021/08_data_test.txt", sep = "|", header = FALSE)
d <- fread("2021/08_data.txt", sep = "|", header = FALSE)

n <- nchar(unlist(strsplit(d$V2, " ")))
tab <- table(n)

sum(tab[as.character(c(2:4,7))])


#---------
# part two
#---------

# d <- fread("2021/08_data_test.txt", sep = "|", header = FALSE)
d <- fread("2021/08_data.txt", sep = "|", header = FALSE)

C1 <- lapply(strsplit(d$V1, " "), function(x) sapply(x, function(y) paste0(sort(strsplit(y, "")[[1]]), collapse = ""), USE.NAMES = FALSE))
C2 <- lapply(strsplit(d$V2, " "), function(x) sapply(x, function(y) paste0(sort(strsplit(y, "")[[1]]), collapse = ""), USE.NAMES = FALSE))

get_ref <- function(x) {
  s <- sapply(x, strsplit, "", USE.NAMES = FALSE)
  l <- lengths(s)
  
  n <- c(1,7,4,8)[match(l, c(2:4,7))]
  
  n[sapply(s, function(x) sum(s[l == 4][[1]] %in% x & length(x) == 6)) == 4] <- 9
  
  i0 <- sapply(s[l==6 & !(n %in% 9)], function(x) length(setdiff(x, s[[which(l==3)]]))) == 3
  n[which(l == 6 & is.na(n))][i0] <- 0
  
  n[which(l == 6 & is.na(n))] <- 6
  
  n[sapply(s, function(x) sum(s[l == 2][[1]] %in% x & length(x) == 5)) == 2] <- 3
  
  i5 <- sapply(s[is.na(n)], function(x) length(c(unlist(sapply(s[which(n == 9)], function(y) setdiff(y, x) )))) ) == 1
  n[is.na(n)][i5] <- 5
  
  n[is.na(n)] <- 2
  
  setNames(n, sapply(s, paste0, collapse = ""))
}

get_num <- function(x, y) {
  r <- get_ref(x)
  as.integer(paste0(r[match(y, names(r))], collapse = ""))
}

sum(mapply(get_num, C1, C2))

