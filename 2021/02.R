library(data.table)

#---------
# part one
#---------

d <- fread("2021/02_data.txt")
d[, V3 := c("f","d")[1L + (V1 != "forward")]]
d[V1 == "up", V2 := V2 * -1L]
d[, sum(V2), by = V3][, Reduce("*",V1)]

#---------
# part two
#---------

d <- fread("2021/02_data.txt")
d[, V3 := c("f","d")[1L + (V1 != "forward")]]
d[V1 == "up", V2 := V2 * -1L]
d[V3 == "d", aim := cumsum(V2)]
d[, aim := shift(aim, fill = 0L)]
d[V3 == "f", aim := nafill(aim, type = "locf")]
d[V3 == "f", last(cumsum(V2)) * last(cumsum(aim * V2))]
