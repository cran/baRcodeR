#' Make hierarchical ID codes
#'
#' Generate hierarchical ID codes for barcode labels. 
#' Hierarchical codes have a nested structure: e.g. Y subsamples from 
#' each of X individuals. Use \code{\link{uniqID_maker}} 
#' for sequential single-level labels. Can be run in interactive mode, 
#' prompting user for input. The data.frame can be saved as CSV for 
#' (i) the \code{\link{create_PDF}} function to generate printable 
#' QR-coded labels; and (ii) to downstream data collection using spreadsheet, 
#' relational database, etc.
#'
#' @param user logical. Run function using interactive mode (prompts user for 
#' parameter values). Default is \code{FALSE}
#' @param hierarchy list. A list with each element consisting of three members
#'  a vector of three elements (string, beginning value, end value). See examples.
#'  Used only when \code{user=FALSE})
#' @param end character. A string to be appended to the end of each label.
#' @param digits numerical. Default is \code{2}. Number of digits to be printed, 
#' adding leading 0s as needed. This will apply to all levels when \code{user=FALSE}. 
#' When the max number of digits in the ID code is greater than number of digits 
#' defined in \code{digits}, then \code{digits} is automatically increased 
#' to avoid errors. 
#' @export
#' @return data.frame of text labels in the first column, with additional columns 
#' for each level in the hierarchy list, as defined by the user.
#' 
#' @seealso \code{\link{uniqID_maker}}
#' @examples
#' if(interactive()){
#' ## for interactive mode
#' uniqID_hier_maker(user = TRUE)
#' }
#'
#' ## how to make hierarchy list
#'
#' ## create vectors for each level in the order string_prefix, beginning_value,
#' ## end_value and combine in list
#'
#' a <- c("a", 3, 6)
#' b <- c("b", 1, 3)
#' c <- list(a, b)
#' Labels <- uniqID_hier_maker(hierarchy = c)
#' Labels
#'
#' ## add string at end of each label
#' Labels <- uniqID_hier_maker(hierarchy = c, end = "end")
#' Labels
#'


uniqID_hier_maker <- function(user = FALSE, hierarchy, end = NULL, digits = 2){
  # user interaction code
  if(user == TRUE){
    hlevels <- numeric_input("What is the number of levels in hierarchy: ")
    
    strEnd <- switch(fake_menu(c("Yes", "No"), "String at end of label? "), TRUE, FALSE)
    
    if(strEnd){
      end <- string_input("Please enter ending string: ")
    } else {
      end <- ""
    }
    
    digits <- numeric_input("Number of digits to print: ")
    
    # possible inputs

    hierarchy <- vector("list", hlevels)
    
    for(i in seq(1,hlevels)){
      str <- string_input(paste0("Please enter string for level ",i,": "))
      
      startNum <- numeric_input(paste0("Enter the starting number for level ",i,": "))
      
      endNum <- numeric_input(paste0("Enter the ending number for level ",i,": "))
      
      maxNum <- max(startNum,endNum)
      hierarchy[[i]]<-c(str, startNum, endNum)
    }
  }
  # end user input
  # hierarchy format check
  if (is.list(hierarchy) == FALSE) stop("Hierarchy is not in list format. See ?uniqID_hier_maker")
  if (length(unique(vapply(hierarchy, length, integer(1)))) != 1) stop("Hierarchy entries are not of equal length.")
  if (any(vapply(hierarchy, length, integer(1)) != 3)) 
    stop("Each level in hierarchy should have a string, a beginning value and an end value.")
  if (length(hierarchy) == 1) 
    stop("Input list has only one level. Did you forget a level or are you sure you are not looking for uniqIDMaker()?")
  # loop through hierarchy to generate text
  for(i in seq(1,length(hierarchy))){
    str <- hierarchy[[i]][1]
    startNum <- suppressWarnings(as.numeric(hierarchy[[i]][2]))
    endNum <- suppressWarnings(as.numeric(hierarchy[[i]][3]))
    if (is.na(startNum) == TRUE){
      stop(paste0("Invalid starting number on level ", i, ". Please doublecheck your input"))
    }
    if (is.na(endNum) == TRUE){
      stop(paste0("Invalid ending number on level ", i, ". Please doublecheck your input"))
    }
    maxNum <- max(startNum, endNum)
    digitsMax <- max(digits, nchar(paste(maxNum)))
    if (digitsMax > digits){
      warning("Digits specified less than max level number. Increasing number of digits for level")
      digits <- digitsMax
    }
    lvlRange <- c(startNum:endNum)
    line <- paste0(str, "%0", digits, "d")
    Labels <- sprintf(line, rep(lvlRange))
    if (i > 1) {
      ## replicate previous labels by number of elements in present level
      temp_barcode <- rep(barcodes, each = length(startNum:endNum))
      ## rep present level elements so the length matches that of temp_barcode
      temp_label <- rep(Labels, length.out = length(temp_barcode))
      ## paste everything together
      Labels <- paste(temp_barcode, temp_label, sep = "-")

    }
    barcodes <- Labels
  } # end hierarchy making loop
  # makes columns out of hierarchy levels
  label_df <- cbind(barcodes, data.frame(t(sapply(strsplit(barcodes, "-"),c)), stringsAsFactors = FALSE))
  df_names <- sapply(hierarchy, function(x) x[1])
  names(label_df) <- c("label", df_names)
  # dont forget to add the string at the end
  label_df$label <- paste0(label_df$label, end)
  return(label_df)
}

