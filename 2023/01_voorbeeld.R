# part 2:
digit_names <- \() c(
  "one", "two", "three", "four", "five",
  "six", "seven", "eight", "nine"
)

digits <- \() setNames(object = 1:9, nm = digit_names())

is_digit_name <- \(x) x %in% digit_names()

is_single_digit <- \(x) grepl(pattern = "^[0-9]$", x = x)

text_to_digits <- \(char_vec) {
  for (i in seq_along(char_vec)) {
    char <- char_vec[i]
    n <- nchar(char)
    found <- numeric()
    for (j in seq(from = 1, to = n)) {
      for (k in seq(from = j, to = n)) {
        word <- substr(x = char, start = j, stop = k)
        if (is_single_digit(word)) {
          found <- c(found, word)
          next
        }
        if (is_digit_name(word)) {
          found <- c(found, digits()[word])
        }
      }
    }
    char_vec[i] <- paste0(found[1], found[length(found)])
  }
  as.integer(char_vec)
}

text_to_digits(regels) |> sum()

t1 <- text_to_digits(regels)
