input <- readLines("input05.txt")

# Extract the seednumbers
seeds <- as.numeric(unlist(strsplit(gsub("seeds: ", "", input[1]), " ")))
input_clusters <- input[-c(1:2)]
  
# Transform input into list of converters.
# converters:
# $`0`
#       [,1] [,2] [,3]
# [1,]   50   98    2
# [2,]   52   50   48
# 
# $`1`
#       [,1] [,2] [,3]
# [1,]    0   15   37
# [2,]   37   52    2
# [3,]   39    0   15
group    <- cumsum(input_clusters == "")
segments <- split(input_clusters[input_clusters != ""], group[input_clusters != ""])
converters <- lapply(segments, function(x){
  matrix(as.numeric(unlist(strsplit(x[-1], " "))), ncol = 3, byrow = T)
})

# To get the location for a seed, the value (starting with seednumber) iterates through the converters
# In every converter it checks if the value overlaps with the numbers and output the (possibly) converted number
# After going through all the converters, the location is found for a given seed. For star 1 the minimum is found
# For star 2 the direction is the other way, so it starts for a given location number and outputs the seednumber
# It starts from location number 1 and increases every loop. If the seednumber overlaps the seedranges, the whileloop stops
# the converterfunction for 1 and 2 is combined, with the addition that when going from loc->seed, some settings are changed
# The starting_input can either be a single number or a vector of numbers
run_converter <- function(starting_input, seed_to_loc = TRUE){
  # Settings for seed -> location
  converter_numbers <- 1:length(converters)
  colnum <- 2
  direction_multiplier <- -1
  
  # Settings for location -> seed
  if(!seed_to_loc){
    converter_numbers <- rev(converter_numbers)
    colnum <- 1
    direction_multiplier <- 1
  }
  
  # The conversion
  sapply(starting_input, function(x){
    for(i in converter_numbers){
      converter <- converters[[i]]
      trs       <- x >= converter[,colnum] & x <= (converter[,colnum] + converter[,3]- 1)
      if(any(trs)){
        conv  <- converter[trs,]
        adder <- conv[2] - conv[1]
        x     <- x + direction_multiplier * adder
      }else{
        x <- x
      }
    }
    x
  })
}

# Star 1
cat("Day 5 Star 1: ", min(run_converter(seeds, TRUE)))

# Star 2
# Now the seeds input is a table with seed range
# For every loc input it runs the converter and checks if it overlaps the seedranges
# if so, stop the while loop, if not, increase the loc and try again
seedranges <- matrix(seeds, ncol = 2, byrow = T)
loc <- 1
while(TRUE){
  seed <- run_converter(loc, seed_to_loc = FALSE)
  if(any(seed >= seedranges[,1] & seed <= (seedranges[,1] + seedranges[,2]- 1))){
    break
  }
  loc <- loc + 1
}
cat("Day 5 Star 2: ", loc)
