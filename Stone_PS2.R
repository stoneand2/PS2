# Andy Stone
# Problem Set 2
# February 11, 2016

setwd("~/github/PS2") # Setting working directory

#### Problem 1 ####

## Function for calculating Leemis m and Cho Gains d statistics
#'
#' This function takes an input of electoral returns and (optionally) calculates the corresponding 
#' Leemis' m and Cho Gains' d statistics.
#' @param input An object with the class integer or matrix. If matrix, should be all integer values.
#' @param Leemis Takes the value of TRUE or FALSE, where TRUE indicates that the function should
#' calculate and return the Leemis' m statistic.
#' @param ChoGains Takes the value of TRUE or FALSE, where TRUE indicates that the function should
#' calculate and return the Cho Gains' d statistic.
#' 
#' @return A list with three elements: a frequency table of distribution of all first significant 
#' digits of the elements within input, (if Leemis=T) the Leemis' m statistic, (if ChoGains=T) the
#' Cho Gains' d statistic.
#' 
#' @author Andy Stone.

violations_calculator <- function(input, Leemis=T, ChoGains=T){
  # Ensuring user inputs electoral returns in proper manner
  # Checks to see if object input is integer vector or matrix. If not, function stops running, 
  # and provides informative error message
  if (class(input) %in% c("integer","matrix")) {
  } else {
    stop("Please input election returns in integer vector or matrix format.")
  }
  
  # Ensuring that, if a matrix is input, it can be coerced to numeric vector
  # And then doing such a maneuver
  # That is, ensuring the user inputs raw vote totals, not letters or something
  if (class(input) == "matrix") {
    input <- c(input)
    if (class(input) != "integer") {
      stop("Please fix your matrix to remove any non-integer values.")
    }
  }
  
  # Now, turning to calculating our statistics of interest
  # First, creating frequency table of distribution of all first significant digits
  first_digit_dist <- prop.table(table(substr(input, start=1, stop=1)))
  # Then, putting these frequencies -- accounting for zeros -- into a vector called props
  props <- rep(0,9)
  props[as.numeric(names(first_digit_dist))] <- first_digit_dist
  
  # Creating list to output variables to
  final_list <- list(digit_distribution=first_digit_dist)
  
  # Then, calculating the Leemis m statistic if Leemis=T and assigning it to the list
  if (Leemis==T) {
    m <- max(props - log10(1 + 1/c(1:9)))
    final_list <- c(final_list, m_statistic=m)
  }
  # Calculating the Cho-Gains d statistic if ChoGains=T and assigning it to the list
  if (ChoGains==T) {
    d <- sqrt(sum((props - log10(1 + 1/c(1:9)))^2))
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
# a.dataframe <- data.frame(c(1,2,3,4))
# 
# violations_calculator(vote.shares) # works
# violations_calculator(vote.matrix) # works, matrix converted to vector within function
# violations_calculator(wrong.matrix) # won't work, not all integers
# violations_calculator(a.dataframe) # won't work, not a matrix
# violations_calculator(vote.shares, Leemis=T, ChoGains=F) # only returns m statistic





#### Problem 2 ####

## Function for hypothesis testing of Leemis m and Cho Gains d statistics
#'
#' This function takes an input of electoral returns and (optionally) conducts a hypothesis test
#' of electoral fraud utilizing Leemis' m and Cho Gains' d statistics. The null hypothesis is of
#' no electoral fraud. This function recursively calls the violations_calculator() defined above.
#' @param input An object with the class integer or matrix. If matrix, should be all integer values.
#' @param Leemis Takes the value of TRUE or FALSE, where TRUE indicates that the function should
#' test the null hypothesis of no electoral fraud using the Leemis' m statistic.
#' @param ChoGains Takes the value of TRUE or FALSE, where TRUE indicates that the function should
#' test the null hypothesis of no electoral fraud using the Cho Gains' d statistic.
#' 
#' @return A table that denotes the name of each test statistic, the value of the test statistic,
#' asterisks denoting the statistical significance of the test statistic, and a legend explaining
#' the level of statistical significance the asterisks denote.  
#' 
#' @author Andy Stone.

print.benfords <- function(input, Leemis=T, ChoGains=T){
  # Recursively calling earlier function with inputs as specified by this function's arguments
  earlier_list <- violations_calculator(input, Leemis, ChoGains)
  # Critical values for each statistic
  leemis_cutoffs <- c(.851, .967, 1.212)
  cho_gains_cutoffs <- c(1.212, 1.330, 1.569)
  # Vector of stars
  stars <- c("","*","**","***")
  
  if (Leemis==T) {
    leemis_critical <- sum(earlier_list$m_statistic > leemis_cutoffs) 
    leemis_stars <- stars[leemis_critical + 1]
  }
  if (ChoGains==T) {
    chogains_critical <- sum(earlier_list$d_statistic > cho_gains_cutoffs) 
    chogains_stars <- stars[chogains_critical + 1]
  }
  
  our.table <- t(data.frame("* denotes reject null hypothesis of no fraud at p<0.1, **: p<0.05, ***:p<0.01"))
  rownames(our.table) <- c("")
  colnames(our.table) <- "Statistic"  
  
  if (Leemis==T) {
    our.table <- rbind(paste(round(earlier_list$m_statistic,5), leemis_stars, sep=""), our.table)
    rownames(our.table)[1] <- c("Leemis m")
  }
  if (ChoGains==T) {
    our.table <- rbind(paste(round(earlier_list$d_statistic,5), chogains_stars, sep=""), our.table)
    rownames(our.table)[1] <- c("Cho gains d")
  }
  print(our.table)
}

# Seeing what the function does
# print.benfords(vote.matrix)
# votes2 <- as.integer(c(1,1,1,1,1,1,1,1,1,1))
# print.benfords(votes2)

## Function for writing output of print.benfords() to a .csv
#'
#' This function takes an input of electoral returns, optional arguments to conduct hypothesis 
#' tests of electoral fraud utilizing Leemis' m and Cho Gains' d statistics, a directory path
#' within which to save a table with output of these hypothesis tests, and an optional argument
#' to set the name (and type) of the file the table will save to (defaults to table.csv). It
#' recursively calls print.benfords(), which in turn recursively calls violations_calculator().
#' @param input An object with the class integer or matrix. If matrix, should be all integer values.
#' @param Leemis Takes the value of TRUE or FALSE, where TRUE indicates that the function should
#' report the results of the hypothesis test of no electoral fraud using the Leemis' m statistic.
#' @param ChoGains Takes the value of TRUE or FALSE, where TRUE indicates that the function should
#' report the results of the hypothesis test of no electoral fraud using the Cho Gains' d statistic.
#' @param directory The directory on the hard drive in which the table output should be saved.
#' @param filename The file within which the table should be saved in. Default is table.csv.
#' 
#' @author Andy Stone.

sink.benfords <- function(input, Leemis=T, ChoGains=T, directory=NULL, filename="/table.csv"){
  # First, an if else statement to check whether the user inputs a valid directory to save .csv within.
  # If directory exists, proceed with process. Call sink to divert output from print.benfords()
  # into directory and saved into filename. Then call print.benfords(). Then close sink() to end 
  # connection.
  # If directory doesn't exist, stops evaluation of function and asks user to specify valid path.
  if (file.exists(directory) == T) {
    sink(file=paste(directory, filename, sep=""))
    print.benfords(input, Leemis, ChoGains)
    sink()
  } else {
    stop("Invalid directory. Please specify a valid directory to save the file within.")
  } 
}

# sink.benfords(input=votes2, directory="~/github/PS2",filename="/tableoutput.csv") # works
# sink.benfords(input=votes2, directory="~/gearthub/PS2",filename="/tableoutput.csv") # invalid dir






