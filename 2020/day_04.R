library(data.table)

#---------
# part one
#---------

fields <- fread("field, description
byr, Birth Year
iyr, Issue Year
eyr, Expiration Year
hgt, Height
hcl, Hair Color
ecl, Eye Color
pid, Passport ID
cid, Country ID")

d4 <- scan(file = "day_4_data.txt",
           what = character(),
           blank.lines.skip = FALSE)

d4 <- data.table(id = cumsum(d4 == "")[d4 != ""],
                 do.call(rbind, strsplit(d4, ":")))
setnames(d4, 2:3, c("field","code"))

d4[, all(fields$field %in% field) | all(setdiff(fields$field, "cid") %in% field)
   , by = id][, sum(V1)]

#---------
# part two
#---------

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

d4 <- scan(file = "day_4_data.txt",
           what = character(),
           blank.lines.skip = FALSE)

d4 <- data.table(id = cumsum(d4 == "")[d4 != ""],
                 do.call(rbind, strsplit(d4, ":"))
                 )[, dcast(.SD, id ~ V1, value.var = "V2")]

d4[between(as.integer(byr), 1920L, 2002L) &
     between(as.integer(iyr), 2010L, 2020L) &
     between(as.integer(eyr), 2020L, 2030L) &
     {x <- as.integer(gsub("(\\d+).*","\\1", hgt))
     y <- gsub("\\d+","", hgt)
     (between(x, 150L, 193L) & y %in% "cm") | (between(x, 59L, 76L) & y %in% "in")} &
     grepl("^#[0-9a-f]{6}$", hcl) &
     ecl %in% c("amb","blu","brn","gry","grn","hzl","oth") &
     grepl("^[0-9]{9}$", pid)
   , .N]
