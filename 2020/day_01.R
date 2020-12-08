library(data.table)

#---------
# part one
#---------

# with data.table
ass1 <- fread("day_1_data.txt")[, CJ(V1, V2 = V1, unique = TRUE, sorted = FALSE)]
ass1[V1 + V2 == 2020][1, V1 * V2]

#---------
# part two
#---------

ass2 <- fread("day_1_data.txt")[, CJ(V1, V2 = V1, V3 = V1, unique = TRUE, sorted = FALSE)]
ass2[V1 + V2 + V3 == 2020][1, V1 * V2 * V3]
