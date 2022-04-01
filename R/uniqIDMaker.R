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
#' @param ending_string a character string or vector of strings to attach to the label.
#' If a vector is used, all combinations of that vector with a unique label will be produced.
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
#' 
#' ## Using the ending_string to produce labels with unique endings
#' ## this is different from hierarchical labels with two levels as there 
#' ## is no numbering, just the text string
#' 
#' 
#' Labels <- uniqID_maker(string = "string", level = c(1:5), digits = 2, ending_string = "A")
#' Labels
#' 
#' Labels <- uniqID_maker(string = "string", level = c(1:5), 
#'                        digits = 2, ending_string = c("A", "B"))
#' Labels
#' 
#'
#' if(interactive()){
#' ## function using user prompt does not use any of the other parameters
#' Labels <- uniqID_maker(user = TRUE)
#' Labels
#' }



uniqID_maker <- function(user = FALSE, string = NULL, level, digits = 3, ending_string = NULL){
  if (user == TRUE) {
    ## asks for string
    string <- string_input("Please enter string for level: ")
    ## first value of level
    startNum <- numeric_input("Please enter the starting number for level (integer): ")
    ## ending value of level
    endNum <- numeric_input("Enter the ending number for level: ")
    ## finds greatest value
    maxNum <- max(startNum,endNum)
    ## checks what is number of digits in max value
    digitsMax <- nchar(paste(maxNum))
    ## asks for how many digits to print
    digits <- numeric_input("Number of digits to print for level: ")
    level <-c(startNum:endNum)
    
  } 
  if (is.numeric(level) == FALSE) stop("Level is not a string of numbers")
  if (is.numeric(digits) == FALSE) stop("Digits is not a numerical value")
  ## if the number to digits to print is less than the digits in max value, increase digits
  if (nchar(paste(max(level))) > digits){
    warning("Digits specified less than max number. Increasing number of digits.")
    digits <- nchar(paste(max(level)))
  }
  
  if(!is.null(ending_string)){
    line <- paste0(string, "%0", digits, "d")
    label <- sprintf(line, rep(level))
    ind_string <- rep(string, length(rep(level)))
    ind_number <- sprintf(paste0("%0", digits, "d"), rep(level))
    suffix_strings <- expand.grid(label, ending_string)$Var2
    label <- paste(label, suffix_strings, sep = "-")
    label_df <- data.frame(label, ind_string, ind_number, end_string = as.character(suffix_strings), stringsAsFactors = FALSE)
  } else {
    line <- paste0(string, "%0", digits, "d")
    label <- sprintf(line, rep(level))
    ind_string <- rep(string, length(rep(level)))
    ind_number <- sprintf(paste0("%0", digits, "d"), rep(level))
    label_df <- data.frame(label, ind_string, ind_number, stringsAsFactors = FALSE)
  }
  
  
  return(label_df)
  
}




