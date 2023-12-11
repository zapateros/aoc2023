# For star 1 I start at S and check the possible next step; there is always just one possible step.
# The while loop runs till it is at the starting position again
# For every step the pipe and indices are added to the loop dataframe. This makes it quite slow
# For star 1 the maximum distance is just the looplength divided by 2
input <- readLines("input10.txt")
mt    <- matrix(unlist(strsplit(input, "")), nrow = length(input), byrow = T)

# For now manually check what pipe S is and which direction
start <- which(mt == "S", arr.ind = T)
mt[start] <- "|"
directions <- data.frame(y = 1, x = 0)

# Loop through the matrix and create the loop dataframe
indices <- data.frame(y = start[1], x = start[2])
loop    <- data.frame(pipe = "S", y = indices$y, x = indices$x)
while(TRUE){
  indices <- indices + directions
  if(all(indices == start)){
    break
  }
  next_point <- mt[indices$y, indices$x]
  if(next_point == "J"){
    if(directions$x == 1){
      directions$x <- 0
      directions$y <- -1
    }else{
      directions$x <- -1
      directions$y <- 0
    }
  }else if(next_point == "L"){
    if(directions$x == -1){
      directions$x <- 0
      directions$y <- -1
    }else{
      directions$x <- 1
      directions$y <- 0
    }
  }else if(next_point == "7"){
    if(directions$x == 1){
      directions$x <- 0
      directions$y <- 1
    }else{
      directions$x <- -1
      directions$y <- 0
    }
  }else if(next_point == "F"){
    if(directions$x == -1){
      directions$x <- 0
      directions$y <- 1
    }else{
      directions$x <- 1
      directions$y <- 0
    }
  }
  loop <- rbind(loop, data.frame(pipe = next_point, y = indices$y, x = indices$x))
}

# Star 1:
cat("Day 10 star 1: ", nrow(loop)/ 2)

# Star 2
# A point is inside the loop if, when looking to the left or right the amount of barriers is an odd number
# obiously | is a barrier, but also FJ or F--J, or L7 or L----7 (the amount of - inbetween is irrelevant)
# F7 and LJ are not barriers because you can slip past them (they are like half loops)
# So the steps are: take a random point (that is not part of the loop), take all the looppoints in the same y value and 
# x higher (so you look to the right from the view of that point), remove all the -, F7 and LJ
# Then count the amount of | F7 and LJ. If the amount is an odd number, the chosen point is inside the loop
loop  <- loop[order(loop$y, loop$x),]
rows  <- c(min(loop$y):max(loop$y))
count <- 0
for(row in rows){
  pipes      <- loop$pipe[loop$y == row]
  pipes_inds <- loop$x[loop$y == row]
  points     <- setdiff(min(pipes_inds):max(pipes_inds), pipes_inds)
  for(point in points){
    view_right <- paste0(pipes[pipes_inds > point], collapse="")
    vr         <- gsub("FJ|L7", "|", gsub("F7|LJ", "", gsub("-", "", view_right)))
    l          <- nchar(vr)
    if(l %% 2 == 1){
      count <- count + 1
    }
  }
}
cat("Day 10 star 2: ", count)






