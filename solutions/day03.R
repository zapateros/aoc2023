# Probably not the fastest solution but it works and the created dataframes are suitable for more data manipulations
input <- readLines("input03.txt")

# Transform input number data into df
# df:
# number x_start y l
# 153    24      1 3
# 988    29      1 3
# 502    36      1 3

df <- NULL
for(i in 1:length(input)){
  string  <- input[i]
  matches <- gregexpr("\\d+", string)
  numbers <- regmatches(string, matches)[[1]]
  if(length(numbers) != 0){
    df_add <- data.frame(number = numbers, x_start = unlist(matches), y = i, l = attr(matches[[1]], "match.length"))
    df     <- rbind(df, df_add)
  }
}

# Find indexes of all symbols and add cols for star 2. 
# symbols:
# symbol  x  y n_neighbours   prod
# *       28 2 0              1
# #       35 2 0              1
# %       49 2 0              1

symbols <- NULL
for(i in 1:length(input)){
  string   <- input[i]
  matches  <- gregexpr("[^0-9.]", string)
  x_values <- unlist(matches)
  syms     <- regmatches(string, matches)[[1]]
  if(x_values[1] != -1){
    symbols <- rbind(symbols, data.frame(symbol = syms, x = x_values, y = i))
  }
}
symbols$n_neighbours <- 0
symbols$prod <- 1

# calculate both star 1 and 2
# for every number in df it looks if it touches a symbol by creating the indexes around the number (and irrelevant: the indexes of the number itself)
# then it tries to match those indices with the symbols.
# if there's at least one hit, the number is added to the count for star 1
# for star 2 if there's a hit on a '*', the symbols$prod for that found * is multiplied by the number and 1 is added to that symbols$n_neighbours
# in the end all the symbols$prod are summed where the n_neighbours is 2
count <- 0
for(i in 1:nrow(df)){
  rel       <- df[i, ]
  x_indices <- c((rel$x_start - 1):(rel$x_start + rel$l))
  indices   <- data.frame(y = c((rel$y - 1):(rel$y + 1)), x = rep(x_indices, each = 3))
  
  hits <- apply(indices, 1, function(inds){
    any(symbols$x == inds[2] & symbols$y == inds[1])
  })  
  
  if(any(hits)){
    count <- count + as.numeric(rel$number)
    indices_hits <- indices[hits,]
    for(j in 1:nrow(indices_hits)){
      symbols_index <- which(symbols$x == indices_hits$x[j] & symbols$y == indices_hits$y[j]) 
      if(symbols$symbol[symbols_index] == "*"){
        symbols$n_neighbours[symbols_index] <- symbols$n_neighbours[symbols_index] + 1
        symbols$prod[symbols_index] <- symbols$prod[symbols_index] * as.numeric(rel$number)
        symbols$sum[symbols_index] <- symbols$sum[symbols_index] + as.numeric(rel$number)
      }
    }
  }
}

cat("Day 3 Star 1: ", count, "\n")

# symbols output is something like:
# symbol  x  y n_neighbours   prod
# *       28 2 2              609596
# #       35 2 0              1
# %       49 2 0              1
# *       65 2 2              420264

cat("Day 3 Star 2: ", sum(symbols$prod[symbols$n_neighbours == 2]))
