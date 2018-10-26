#' Generate a list of ID codes
#'
#' Create ID codes consisting of a text string and unique numbers (string001, string002, ...). 
#' Can be run in interactive mode, prompting user for input. The data.frame 
#' output can be saved as CSV for (i) the \code{\link{create_PDF}} function 
#' to generate printable QR-coded labels; and (ii) to downstream data 
#' collection software (spreadsheets, relational databases, etc.)
#'
#' When the function is called with \code{user = TRUE}, a sequence of 
#' numbers is generated between the starting and ending number provided by the 
#' user. When \code{user = FALSE}, a vector of custom numbers can be provided. 
#' See example below.
#'
#' @return data.frame with text labels in the first column, along with string
#' and numeric values in two additional columns. 
#' @param user logical. Run function using interactive mode (prompts user for 
#' parameter values). Default is \code{FALSE}
#' @param string character. Text string for label. Default \code{null}.
#' @param level integer vector. Defines the numerical values to be appended
#' to the character string. Can be any sequence of numbers (see examples).
#' @param digits numerical. Default is \code{2}. Number of digits to be printed, 
#' adding leading 0s as needed. This will apply to all levels when \code{user=FALSE}. 
#' When the numeric value of the label has a greater number of digits than 
#' \code{digits}, \code{digits} is automatically increased for the entire level. 
#' Default is \code{3}.
#'
#' @seealso \code{\link{uniqID_hier_maker}}
#' @export
#' @examples
#'
#'
#' ## sequential string of numbers in label
#' Labels <- uniqID_maker(string = "string", level = c(1:5), digits = 2)
#' Labels
#' 
#' ## can also use nonsequential strings in input for levels
#' level <- c(1:5, 8:10, 999:1000)
#' Labels <- uniqID_maker(string = "string", level = level, digits = 4)
#' Labels
#'
#' if(interactive()){
#' ## function using user prompt does not use any of the other parameters
#' Labels <- uniqID_maker(user = TRUE)
#' Labels
#' }



uniqID_maker <- function(user = FALSE, string = NULL, level, digits = 3){
  if (user == TRUE) {
    ## asks for string
    string <- readline(paste0("Please enter string for level: "))
    ## first value of level
    startNum <- as.numeric(readline(paste0("Enter the starting number for level: ")))
    while(is.na(startNum) == TRUE) {
      print("Invalid input. Please enter an integer.")
      startNum <- as.numeric(readline(paste0("Enter the starting number for level: ")))
    }
    ## ending value of level
    endNum <- as.numeric(readline(paste0("Enter the ending number for level: ")))
    while(is.na(endNum) == TRUE){
      print("Invalid input. Please enter an integer.")
      endNum <- as.numeric(readline(paste0("Enter the ending number for level: ")))
    }
    ## finds greatest value
    maxNum <- max(startNum,endNum)
    ## checks what is number of digits in max value
    digitsMax <- nchar(paste(maxNum))
    ## asks for how many digits to print
    digits <- as.numeric(readline(paste0("Number of digits to print for level: ")))
    # check input
    while(is.na(as.numeric(digits)) == TRUE){
      print("Invalid input. Please only enter an integer.")
      digits <- as.numeric(readline(paste0("Number of digits to print: ")))
    }
    ## if the number to digits to print is less than the digits in max value, try again
    while (digits<digitsMax){
      print(paste0("Please enter a number larger or equal to ", nchar(paste(maxNum))))
      digits <- as.numeric(readline(paste0("Number of digits to print for level: ")))
    }
    level <-c(startNum:endNum)
  }
  if (is.numeric(level) == FALSE) stop("Level is not a string of numbers")
  if (is.numeric(digits) == FALSE) stop("Digits is not a numerical value")
  if (nchar(paste(max(level))) > digits){
    warning("Digits specified less than max number. Increasing number of digits.")
    digits <- nchar(paste(max(level)))
  }
  line <- paste0(string, "%0", digits, "d")
  label <- sprintf(line, rep(level))
  ind_string <- rep(string, length(rep(level)))
  ind_number <- sprintf(paste0("%0", digits, "d"), rep(level))
  return(data.frame(label, ind_string, ind_number))

}



