# First the input is parsed 
input        <- readLines("input19.txt")
n            <- which(input == "")
instrs       <- input[1:(n-1)]
coords       <- input[(n+1): length(input)]
instructions <- list()
for(i in 1:length(instrs)){
  instr <- unlist(strsplit(instrs[i], "\\{|}|\\,"))
  add_all <- list()
  l <- length(instr)
  for(j in 2:(l-1)){
    if(j == (l-1)){
      addx <- unlist(strsplit(instr[j], ":"))
      if(grepl(">", addx[1])){
        addy <- unlist(strsplit(addx[1], ">"))
        add <- c(addy[1], ">", addy[2], addx[2], instr[l])
      }else if(grepl("<", addx[1])){
        addy <- unlist(strsplit(addx[1], "<"))
        add <- c(addy[1], "<", addy[2], addx[2], instr[l])
      }
    }else{
      addx <- unlist(strsplit(instr[j], ":"))
      add1 <- addx[1]
      add2 <- addx[2]
      if(grepl(">",add1)){
        addy <- unlist(strsplit(add1, ">"))
        add <- c(addy[1], ">", as.numeric(addy[2]), add2)
      }else if(grepl("<",add1)){
        addy <- unlist(strsplit(add1, "<"))
        add <- c(addy[1], "<", as.numeric(addy[2]), add2)
      }
    }
    add_all[[j-1]] <- add
  }
  instructions[[instr[1]]] <- add_all
}

# Star 1
sm <- 0
for(k in 1:length(coords)){
  coordx <- coords[k]
  x <- unlist(strsplit(coordx, "\\{|\\=|\\,|}"))
  coord <- list()
  coord[["x"]] <- as.numeric(x[which(x == "x") + 1])
  coord[["m"]] <- as.numeric(x[which(x == "m") + 1])
  coord[["a"]] <- as.numeric(x[which(x == "a") + 1])
  coord[["s"]] <- as.numeric(x[which(x == "s") + 1])
  
  next_instr <- "in"
  while(TRUE){
    ins <- instructions[[next_instr]]
    l <- length(ins)
    i <- 1
    while(TRUE){
      tr <- ins[[i]]
      if(i < l){
        if(eval(parse(text = paste0(coord[[tr[1]]], tr[2], tr[3], collapse = "")))){
          next_instr <- tr[4]
          break
        }else{
          i <- i +1
        }
      }else{
        if(eval(parse(text = paste0(coord[[tr[1]]], tr[2], tr[3], collapse = "")))){
          next_instr <- tr[4]
        }else{
          next_instr <- tr[5]
        }
        break
      }
    }
    
    if(next_instr %in% c("R", "A")){
      if(next_instr == "A"){
        sm <- sm + sum(unlist(coord)) 
      }
      break
    }
  }
}
cat("Day 19 Star 1: ", sm)

# Star 2
# For the second star you start at "in" and from there go recursive into the possible next steps, until you end at an A or an R.
# with every step the possible group of x, m, a or s is getting smaller. Every group starts with possibles from 1 to 4000. for example:
# s>3448:A,lnx means that for all s's greater than 3448 an A is found for s <= 3448 it goes to the next step lnx
# For the first step (to A) now the boundaries (s_min, s_max) are (3449, 4000) (if the s-boundaries before were (1,4000))
# if the boundaries for example were (3700, 3900), the stay the same because if s was 3449 it wouldnt even come to that step
# When at the end of a path an A is found, a number n is added to the total, n = (x_max - x_min +1) * (m_max - m_min +1) * (a_max - a_min +1) * (s_max - s_min +1)
run <- function(step, limits){
  if(step %in% c("A", "R")){
    if(step == "A"){
      count <<- count + prod(unlist(sapply(limits, function(x){x[2] - x[1] + 1})))
    }
  }else{
    instruction <- instructions[[step]]
    l <- length(instruction)
    for(i in 1:l) {
      instr <- instruction[[i]]
      limits <- process_instruction(instr, limits)
    }
  }
}

process_instruction <- function(instr, limits) {
  limits1 <- limits
  limits2 <- limits
  
  if(instr[2] == "<") {
    limits1[[instr[1]]]$max <- min(c(limits1[[instr[1]]]$max, as.numeric(instr[3]) - 1))
    limits2[[instr[1]]]$min <- max(c(limits2[[instr[1]]]$min, as.numeric(instr[3])))
  } else {
    limits1[[instr[1]]]$min <- max(c(limits1[[instr[1]]]$min, as.numeric(instr[3]) + 1))
    limits2[[instr[1]]]$max <- min(c(limits2[[instr[1]]]$max, as.numeric(instr[3])))
  }
  
  run(instr[4], limits1)
  if(length(instr) > 4) {
    run(instr[5], limits2)
  }
  
  return(limits2)
}


limits <- list(x = data.frame(min = 1, max = 4000),
               m = data.frame(min = 1, max = 4000),
               a = data.frame(min = 1, max = 4000),
               s = data.frame(min = 1, max = 4000))

step   <- "in"
count <- 0
run(step, limits)
cat("Day 19 Star 2: ", as.character(count))



