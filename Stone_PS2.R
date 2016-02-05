# Andy Stone
# Problem Set 2
# February 11, 2016

#### Problem 1 ####

# The function should take as an input (i) a matrix or vector of election returns and 
# (ii) an option (or options) that controls whether the m statistic should be calculated, 
# the d statistic should be calculated, or both. 

# The output should be a list containing the results, including the full digit distribution.

violations_calculator <- function(input, Leemis=T, ChoGains=T){
  # Ensuring user inputs electoral returns in proper manner
  # Checks to see if object input is integer vector or matrix. If not, function stops running, 
  # and provides informative error message
  if(class(input) %in% c("integer","matrix")) {
  } else {
    stop("Please input election returns in integer vector or matrix format.")
  }
  
  # Ensuring that, if a matrix is input, it can be coerced to numeric vector
  # And then doing such a manouver
  # That is, ensuring the user inputs raw vote totals, not letters or something
  if(class(input) == "matrix") {
    input <- c(input)
    if(class(input) != "integer"){
      stop("Please fix your matrix to remove any non-integer values.")
    }
  }
  
  # Now, turning to calculating our statistics of interest
  # First, creating frequency table of distribution of all first significant digits
  first_digit_dist <- prop.table(table(substr(input, start=1, stop=1)))
  
  # Creating list to output variables to
  final_list <- list(digit_distribution=first_digit_dist)
  
  # Then, calculating the Leemis m statistic if Leemis=T and assigning it to the list
  if(Leemis==T){
    m <- max(first_digit_dist - log10(1 + 1/c(1:9)))
    final_list <- c(final_list, m_statistic=m)
  }
  # Calculating the Cho-Gains d statistic if ChoGains=T and assigning it to the list
  if(ChoGains==T){
    d <- sqrt(sum((first_digit_dist - log10(1 + 1/c(1:9)))^2))
    final_list <- c(final_list, d_statistic=d)
  }
  
  # Output. Distribution table will always be reported
  return(final_list)  
} 

# Checking to see how the function does 
# set.seed(10)
# vote.shares <- sample(10000:99999, 50, replace=T)
# 
# vote.matrix <- matrix(vote.shares, nrow=10)
# wrong.matrix <- matrix(c("a","b","c","d"), nrow=2)
# 
# violations_calculator(vote.shares) # works
# violations_calculator(vote.matrix) # works, matrix converted to vector within function
# violations_calculator(wrong.matrix) # won't work, not all integers
# violations_calculator(vote.shares, Leemis=T, ChoGains=F)


#### Problem 2 ####
print.benfords <- function(){
  our.table <- t(data.frame("0.083***", "0.257","* denotes p<0.1, **: p<0.05, ***:p<0.01"))
  rownames(our.table) <- c("Leemis m","Cho-Gains d","")
  colnames(our.table) <- "Statistic"
  
  
}

# rbind(table(final.list), c("1***",2,3,4,5))





