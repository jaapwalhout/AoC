
#-------
# part 1
#-------

# d <- scan(file = "2023/08_data_test1.txt", what = character(), sep = "\n")
# d <- scan(file = "2023/08_data_test2.txt", what = character(), sep = "\n")

d <- scan(file = "2023/08_data.txt", what = character(), sep = "\n")

instr <- chartr("LR", "12", d[1]) |> strsplit(split = "") |> unlist() |> as.integer()

node_nms <- sub("([A-Z]{3})\\s=.*", "\\1", d[-1])

nodes <- sub(".*=\\s\\(([A-Z]{3}).*([A-Z]{3})\\)$", "\\1,\\2", d[-1]) |>
  strsplit(split = ",")
names(nodes) <- node_nms

l <- length(instr)
steps <- 0
node <- "AAA"
while(node != "ZZZ") {
  i <- (steps %% l) + 1
  node <- nodes[[node]][instr[i]]
  steps <- steps + 1
}
steps


#-------
# part 2
#-------

# d <- scan(file = "2023/08_data_test3.txt", what = character(), sep = "\n")

d <- scan(file = "2023/08_data.txt", what = character(), sep = "\n")

instr <- chartr("LR", "12", d[1]) |> strsplit(split = "") |> unlist() |> as.integer()

node_nms <- sub("([A-Z0-9]{3})\\s=.*", "\\1", d[-1])

nodes <- sub(".*=\\s\\(([A-Z0-9]{3}).*([A-Z0-9]{3})\\)$", "\\1,\\2", d[-1]) |>
  strsplit(split = ",")
names(nodes) <- node_nms

l <- length(instr)
steps <- 0
node <- node_nms[substr(node_nms,3,3) == "A"]
while(!all(substr(node,3,3) == "Z")) {
  i <- (steps %% l) + 1
  node <- sapply(nodes[node], `[`, instr[i]) |> unique()
  steps <- steps + 1
}
steps
