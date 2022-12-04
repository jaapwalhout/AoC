
library(data.table)

#-------
# part 1
#-------

# rock      A  X
# paper     B  Y
# scissors  C  Z
# 
# rock > scissors
# scissors > paper
# paper > rock

d <- fread("2022/02_data.txt", header = FALSE)

d[, score := match(V2, LETTERS[24:26])]

d[, outcome := fcase((V1 == "A" & V2 == "X") | (V1 == "B" & V2 == "Y") | (V1 == "C" & V2 == "Z"), 3,
                     V1 == "A" & V2 == "Y", 6,
                     V1 == "A" & V2 == "Z", 0,
                     V1 == "B" & V2 == "X", 0,
                     V1 == "B" & V2 == "Z", 6,
                     V1 == "C" & V2 == "X", 6,
                     V1 == "C" & V2 == "Y", 0)]

d[, sum(score + outcome)]

#-------
# part 2
#-------

d <- fread("2022/02_data.txt", header = FALSE)

d[, V3 := fcase(V2 == "X" & V1 == "A", "Z",
                V2 == "X" & V1 == "B", "X",
                V2 == "X" & V1 == "C", "Y",
                V2 == "Y" & V1 == "A", "X",
                V2 == "Y" & V1 == "B", "Y",
                V2 == "Y" & V1 == "C", "Z",
                V2 == "Z" & V1 == "A", "Y",
                V2 == "Z" & V1 == "B", "Z",
                V2 == "Z" & V1 == "C", "X")]

d[, score := match(V3, LETTERS[24:26])]

d[, outcome := fcase((V1 == "A" & V3 == "X") | (V1 == "B" & V3 == "Y") | (V1 == "C" & V3 == "Z"), 3,
                     V1 == "A" & V3 == "Y", 6,
                     V1 == "A" & V3 == "Z", 0,
                     V1 == "B" & V3 == "X", 0,
                     V1 == "B" & V3 == "Z", 6,
                     V1 == "C" & V3 == "X", 6,
                     V1 == "C" & V3 == "Y", 0)]

d[, sum(score + outcome)]

