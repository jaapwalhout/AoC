
library(data.table)
library(stringi)

#---------
# part one
#---------

# raw_input <- readLines("2021/14_data_test.txt")
raw_input <- readLines("2021/14_data.txt")

p <- raw_input[1]
d <- setDT(tstrsplit(raw_input[3:length(raw_input)], split = " -> "))
d[, V3 := paste0(substr(V1,1,1), V2 ,substr(V1,2,2))]

n <- 10 

for (i in 1:n) {
  ix <- stri_detect_fixed(p, d$V1)
  tmp <- d[ix]
  
  strlocs <- stri_locate_all_fixed(p, tmp$V1, overlap = TRUE)
  it <- rep(1:nrow(tmp), lengths(strlocs)/2)
  tmp <- tmp[it]
  loc <- do.call(rbind, strlocs)
  ord <- order(loc[,1])
  loc <- loc[ord,]
  loc <- sweep(loc, 1, (1:nrow(loc)) - 1, '+')
  tmp <- tmp[ord]
  
  for (j in 1:nrow(tmp)) {
    p <- stri_sub_replace(p, from = loc[j,1], to = loc[j,2],
                          replacement = tmp[j, V3])
  }
}

tab <- table(strsplit(p, ""))
diff(range(tab))


#---------
# part two
#---------

raw_input <- readLines("2021/14_data_test.txt")
# raw_input <- readLines("2021/14_data.txt")

p <- raw_input[1]
d <- setDT(tstrsplit(raw_input[3:length(raw_input)], split = " -> "))
p;d

ref <- data.table(setDT(tstrsplit(d$V1, "")), d$V2)[, `:=` (ID = .I, N = 0)][]
setnames(ref, old = 1:3, new = paste0("V",1:3))
ref

s <- strsplit(p, NULL)[[1]]
s1 <- data.table(V1 = head(s, -1), V2 = tail(s, -1))
s1

s2 <- ref[s1, on = .(V1, V2)]

lp <- s1[.N, .(V1, V2)]

tmp <- d[s, on = .(c1, c2), .(c1, c2, c3, n = n + 1)]
s2[, c(matrix(c(V1,V3), nrow = 2, byrow = TRUE), tail(V2, 1))]

ref <- p

n <- 5

for (i in 1:n) {
  
  
}

steps <- fread(text = "step 1: NCNBCHB
step 2: NBCCNBBBCBHCB
step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB", sep = ":", h = F)
steps

sapply(steps$V2, \(x) table(strsplit(x, "")), USE.NAMES = FALSE)
