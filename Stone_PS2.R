# Andy Stone
# Problem Set 2
# February 11, 2016

# The function should take as an input (i) a matrix or vector of election returns and 
# (ii) an option (or options) that controls whether the m statistic should be calculated, 
# the d statistic should be calculated, or both. 

# The output should be a list containing the results, including the full digit distribution.

# Practice vector of electoral returns
set.seed(10)
vote.shares <- sample(10000:99999, 50, replace=T)

vote.matrix <- matrix(vote.shares, nrow=10)
wrong.matrix <- matrix(c("a","b","c","d"), nrow=2)

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
  print(input)
} 



