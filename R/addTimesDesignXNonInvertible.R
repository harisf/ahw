addTimesDesignXNonInvertible <- function(aalenFit, consoleMessagesFilePath = NULL, outFileLines = NULL){
  if(is.null(consoleMessagesFilePath) & is.null(outFileLines))
    stop("Either provide the file path or the character vector containing the output messages from timereg::aalen.")
  
  # regex pattern that matches a number of the form 3.141593
  digitPattern <- "\\d.\\d+"
  
  if(!is.null(consoleMessagesFilePath))
    outFileLines <- readLines(consoleMessagesFilePath)
  
  timesDesignXNonInvertible <- list()
  
  for(l in seq_along(outFileLines)) {
    
    line <- outFileLines[[l]]
    
    # If there is an "Error in invert..." message
    if(grepl("Error in invert", line)){
      
      # the next line should contain the time  when "X'X not invertible"
      nextLine <- outFileLines[[l+1]]
      
      if(grepl("X'X not invertible", nextLine)){
        
        digitMatch <- regexpr(digitPattern, nextLine)
        time <- regmatches(nextLine, digitMatch)
        
        # add time to list of all times where X'X is not invertible
        timesDesignXNonInvertible <- c(timesDesignXNonInvertible,
                                       as.numeric(time))
      }
    }
  }
  attr(aalenFit, "timesDesignXNonInvertible") <- unlist(timesDesignXNonInvertible)
  return(aalenFit)
}

# # More compact implementation
# addTimesDesignXNonInvertible <- function(aalenFit, consoleMessagesFilePath){
#   outFileLines <- readLines(consoleMessagesFilePath)
#   
#   if(length(outFileLines) != 0){
#     
#     # keep every second line
#     outFileLines <- outFileLines[seq(2, length(outFileLines), by = 2)]
#     
#     timesDesignXnonInvertible <- vapply(outFileLines, function(txt, pattern){
#       as.numeric(regmatches(txt, regexpr(pattern, txt)))
#     }, FUN.VALUE = 0, pattern = "\\d.\\d+", USE.NAMES = F)
#   }
#   
#   aalenFit$timesDesignXnonInvertible <- timesDesignXnonInvertible
#   return(aalenFit)
# }