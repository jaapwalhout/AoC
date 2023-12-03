
#-------
# part 1
#-------

# regels <- scan(text = "1abc2
# pqr3stu8vwx
# a1b2c3d4e5f
# treb7uchet", what = character())

regels <- scan(file = "2023/01_data.txt", what = character())

gregexpr(pattern = "\\d", text = regels) |> regmatches(x = regels)

extract_values <- \(x) {
  g <- gregexpr(pattern = "\\d", text = x)
  r <- regmatches(x = x, m = g)
  v <- sapply(r, \(i) as.integer(paste0(head(i, 1), tail(i, 1))))
  return(v)
}

extract_values(regels) |> sum()

#-------
# part 2
#-------

library(stringi)
library(data.table)

# regels <- scan(text = "two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen", what = character())

regels <- scan(file = "2023/01_data.txt", what = character())

cijfers <- c("zero","one","two","three","four","five","six","seven","eight","nine")

rev_string <- \(x) {
  s <- strsplit(x, "")
  s <- lapply(s, rev)
  s <- sapply(s, paste0, collapse = "")
  s
}

pat_first <- paste0(c("\\d", cijfers), collapse = "|")
s1 <- stri_extract_first_regex(regels, pattern = pat_first)
s1 <- fcoalesce(as.numeric(s1), match(s1, cijfers) - 1)

pat_last <- paste0(c("\\d", rev_string(cijfers)), collapse = "|")
s2 <- stri_extract_first_regex(rev_string(regels), pattern = pat_last)
s2 <- fcoalesce(as.numeric(s2), match(rev_string(s2), cijfers) - 1)

as.integer(paste0(s1, s2)) |> sum()
