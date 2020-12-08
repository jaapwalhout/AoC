
#---------
# part one
#---------

d6 <- scan(file = "06_data.txt",
           what = character(),
           blank.lines.skip = FALSE)
d6 <- split(d6[d6 != ""], cumsum(d6 == "")[d6 != ""])

sum(sapply(d6, function(x) length(unique(unlist(strsplit(x, ""))))))


#---------
# part two
#---------

# txt_con <- textConnection("abc
#
# a
# b
# c
#
# ab
# ac
#
# a
# a
# a
# a
#
# b")
#
# d6 <- scan(file = txt_con,
#            what = character(),
#            blank.lines.skip = FALSE)
# d6 <- split(d6[d6 != ""], cumsum(d6 == "")[d6 != ""])
# d6

sum(lengths(lapply(lapply(d6, strsplit, ""), function(x) Reduce(base::intersect, x))))

