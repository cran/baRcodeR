.onAttach <- function(libname, pkgname){
  options(baRcodeR.connection = stdin())
}

fake_menu <- function(choices, prompt_string){
  attempt <- 1
  attempts_allowed <- 3
  # real menu doesn't work with std in
  choice_string <- paste(1:length(choices), ":", choices, collapse = "\n")
  repeat {
    choice <- numeric_input(paste0(prompt_string, "\n", choice_string))
    if (choice == 0 | choice > length(choices)){
      cat(paste0("Invalid input. Integer must be between 1 and ", length(choices)))
    }
    attempt <- attempt + 1
    if(attempt > attempts_allowed){
      stop("Invalid input. Please try again.")
    }
    if (choice <= length(choices) & choice != 0){
     break 
    }
  }
  return(choice)
}


numeric_input <- function(prompt_string, attempts_allowed = 3, integer = TRUE){
  attempt <- 1
  repeat {
    cat(prompt_string)
    startNum <- suppressWarnings(abs(as.numeric(readLines(con = getOption("baRcodeR.connection"), n = 1))))
    if(integer){
      startNum <- as.integer(startNum)
    }
    attempt <- attempt + 1
    # exit if input is number
    if (!is.na(startNum)) {
      break
    }
    # exit if too many attempts
    if(attempt > attempts_allowed){
      stop("Invalid input. Please try again.")
    } else {
      cat("Invalid input. Please enter a number")
    }
  }
  ifelse(!is.na(startNum), return(startNum))
  
}


string_input <- function(prompt_string){
  cat(prompt_string)
  return(readLines(con = getOption("baRcodeR.connection"), n = 1))
}
