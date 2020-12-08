library(data.table)

#---------
# part one
#---------

d7 <- readLines(textConnection("light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

d7 <- readLines("07_data.txt")
d7 <- rbindlist(lapply(strsplit(d7, "contain|,"),
                       function(x) setNames(as.list(x), paste0("V", seq_along(x)))),
                fill = TRUE)

# bag_colors <- d7[V2 %like% "shiny gold" | V3 %like% "shiny gold", trimws(sub("bags", "", V1))]
#
# d7[V2 %like% bag_colors | V3 %like% bag_colors, trimws(sub("bags", "", V1))]
# d7[sapply(bag_colors, function(x) V2 %like% x) | V3 %like% bag_colors
#    , trimws(sub("bags", "", V1))]
#
# d7[, !!rowSums(sapply(bag_colors, function(x) V2 %like% x))]
# d7[, sapply(bag_colors, function(x) V2 %like% x)]
# d7[, sapply(bag_colors, function(x) V3 %like% x)]
# d7[, sapply(bag_colors, function(x) V2 %like% x | V3 %like% x)]
# d7[, !!rowSums(sapply(bag_colors, function(x) V2 %like% x | V3 %like% x))]


add <- 1

search_colors <- "shiny gold"

bag_colors <- vector(mode = "character")

counter <- 1L

while(add == 1) {
  print(sprintf("Run: %s", counter))
  print(sprintf(" - Search colors: %s", paste0(search_colors, collapse = ", ")))

  i <- d7[, !!rowSums(sapply(search_colors, function(x) V2 %like% x | V3 %like% x | V4 %like% x | V5 %like% x))]
  add_colors <- d7[i, trimws(gsub("bags", "", V1))]
  print(sprintf(" - Added colors: %s", paste0(add_colors, collapse = ", ")))

  bag_colors <- c(bag_colors, add_colors)
  print(sprintf(" - Bag colors: %s", paste0(bag_colors, collapse = ", ")))

  search_colors <- add_colors

  if (length(search_colors) < 1) {
    add <- 0
    print("Finished!")
    break
  } else { print("----------\n") }
  counter <- counter + 1L
}

length(unique(bag_colors))



#---------
# part two
#---------

# d7 <- readLines(textConnection("light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted black bags.
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
# faded blue bags contain no other bags.
# dotted black bags contain no other bags."))

d7 <- readLines("07_data.txt")

d7 <- rbindlist(lapply(strsplit(d7, "contain|,"),
                       function(x) setNames(as.list(x), paste0("V", seq_along(x)))),
                fill = TRUE)
d7 <- melt(d7, id = 1)[, .(from = trimws(gsub("bags","",V1)),
                           to = trimws(gsub("bags|bag|\\.|\\d+","",value)),
                           value = as.integer(gsub(".*(\\d+).*","\\1",value)))
                       ][!is.na(value)]

add <- 1

search_colors <- setNames(1L, "shiny gold")

count <- 0

while(add == 1) {
  i <- d7[, .I[from %in% names(search_colors)]]

  plus <- setDT(stack(search_colors))[d7[i]
                , on = .(ind = from)
                , .(to, value2 = value * values)]

  count <- count + plus[, sum(value2)]

  search_colors <- plus[, setNames(value2, to)]

  if (length(search_colors) < 1) add <- 0

}

count

