library(data.table)
library(stringi)

#---------
# part one
#---------

# with data.table
ass1 <- fread("day_2_data.txt", header = FALSE)
ass1[, V2 := sub(":","",V2)]
ass1[, c("mini","maxi") := tstrsplit(V1, "-", type.convert = TRUE)]

ass1[, sum(between(stri_count_fixed(V3, V2), mini, maxi))]

#---------
# part two
#---------

pos <- Map(grep, ass1$V2, strsplit(ass1$V3, ""))
sum(mapply(function(x, y, z) sum(x %in% c(y, z)) == 1L,
           pos, ass1$mini, ass1$maxi))
