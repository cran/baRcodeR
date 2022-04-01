#' Make barcodes and print labels
#'
#' Input a vector or data frame of ID codes to produce a PDF of barcode labels
#' that can then be printed. The PDF setup is for the ULINE 1.75" * 0.5" WEATHER
#' RESISTANT LABEL for laser printer; item # S-19297 (uline.ca). See details for
#' how to format text labels properly.
#'
#' \code{qrcode_make} is the helper function for generating a QR code matrix.
#' \code{code_128_make} is the helper function for generating a linear barcode
#' according to code 128 set B. \code{code_128_make2} is the extended helper function for generating a linear barcode
#' according to code 128 set B, all Latin-1 characters, set C and partially A. \code{custom_create_PDF} is the main function
#' which sets page layout, and creates the PDF file.
#' 
#'
#' Correction levels for QR codes refer to the level of damage a label can
#' tolerate before the label become unreadable by a scanner (L = Low (7\%), M =
#' Medium (15\%), Q = Quantile (25\%), H = High (30\%)). So a label with L
#' correction can lose up to at most 7% of the code before it is unreadable
#' while a H label can lose up to 30% of the code. This also means that L codes
#' can be printed at smaller sizes compared to H codes.
#' 
#' The escape characters \code{\\n} and \code{\\s} (and the hex equivalents
#' \code{\\x0A} and \code{\\x20} can be used to format text labels. Tab character
#' \code{\\t} (\code{\\x09}) does not work for QR codes and should be replaced by
#' a number of space characters. See the package vignette for examples.
#' 
#' If \code{ECol} or \code{ERow} is greater than \code{numcol} and \code{numrow}, 
#' the labels will be printed starting on the second page. 
#'
#' @return a PDF file containing QR-coded labels, saved to the default
#'   directory.
#'
#' @param user logical. Run function using interactive mode (prompts user for
#'   parameter values) Default is \code{FALSE}
#' @param Labels vector or data frame object containing label names (i.e. unique
#'   ID codes) with either UTF-8 or ASCII encoding.
#' @param alt_text vector containing alternative names that are printed along with 
#'   Labels BUT ARE NOT ENCODED in the barcode image. Use with caution!  
#' @param replace_label logical. Replace label text with \code{alt_text}.
#'   Generated barcode will contain more information than text label. Use with
#'   caution!
#' @param denote character (prefix) or vector of length 2 (prefix, suffix). 
#'   Denotes alt_text that is not encoded in the barcode image. 
#'   Default is brackets before and after ().  
#' @param name character. Name of the PDF output file. Default is
#'   \code{"LabelsOut"}. A file named \code{name.pdf} will be saved to the
#'   working directory by default. Use \code{"dirname/name"} to produce a file
#'   called \code{name.pdf} in the \code{dirname} directory.
#' @param type character. Choice of \code{"linear"} for code 128, or \code{"linear2"}
#'   for extended code 128, or \code{"matrix"}
#'   for QR code (i.e. 2D barcode) labels. Default is \code{"matrix"}.
#' @param ErrCorr error correction value for matrix labels only. Level of damage
#'   from low to high: \code{"L"}, \code{"M"}, \code{"Q"}, \code{"H"}. Default
#'   is \code{"H"}. See details for explanation of values.
#' @param Fsz numerical. Sets font size in points. Longer ID codes may be shrunk
#'   to fit if truncation is not used for matrix labels. Default font size is
#'   \code{5}. ID codes are also shrunk automatically to fit on the label if
#'   actual size is bigger than label dimensions.
#' @param Across logical. When \code{TRUE}, print labels across rows, left to
#'   right. When \code{FALSE}, print labels down columns, top to bottom. Default
#'   is \code{TRUE}.
#' @param ERows number of rows to skip. Default is \code{0}. Example: setting
#'   ERows to 6 will begin printing at row 7. ERows and ECols are useful for
#'   printing on partially-used label sheets.
#' @param ECols number of columns to skip. Default is \code{0}. Example: setting
#'   ECols to 2 will put the first label at column 3. ERows and ECols are useful
#'   for printing on partially-used label sheets.
#' @param trunc logical. Text is broken into multiple lines for longer ID codes,
#'   to prevent printing off of the label area. Default is \code{TRUE}. If
#'   \code{trunc = FALSE}, and text is larger than the physical label, the text will
#'   be shrunk down automatically.
#' @param numrow numerical. Number of rows per page. Default is \code{20}.
#' @param numcol numerical. Number of columns per page. Default is \code{4}.
#' @param page_width numerical. Width of page (in inches). Default is set to
#'   \code{8.5}.
#' @param page_height numerical. Height of page (in inches). Default is set to
#'   \code{11}.
#' @param width_margin numerical. The width margin of the page (in inches).
#'   Default is \code{0.25}.
#' @param height_margin numerical. The height margin of the page (in inches).
#'   Default is \code{0.5}.
#' @param label_width numerical. The width of label (in inches). Will be
#'   calculated as \code{(page_width - 2 * width_margin)/numcol} if
#'   \code{label_width} is set as \code{NULL}.
#' @param label_height numerical. The height of the label (in inches). Will be
#'   calculated as \code{(page_height - 2 * height_margin)/numrow} if
#'   \code{label_height} is set as \code{NULL}.
#' @param x_space numerical. A value between \code{0} and \code{1}. This sets
#'   the distance between the QR code and text of each label. Only applies when
#'   \code{type = "matrix"}. Default is \code{0}.
#' @param y_space numerical. The height position of the text on the physical
#'   label as a proportion of the label height. Only applies when \code{type =
#'   "matrix"}. A value between \code{0} and \code{1}. Default is \code{0.5}.
#'   
#'   
#' @examples
#' 
#' ## this is the same examples used with create_PDF
#' ## data frame
#' example_vector <- as.data.frame(c("ao1", "a02", "a03"))
#'
#' \dontrun{
#' ## run with default options
#' ## pdf file will be "example.pdf" saved into a temp directory
#' temp_file <- tempfile()
#' 
#' custom_create_PDF(Labels = example_vector, name = temp_file)
#' 
#' ## view example output from temp folder
#' system2("open", paste0(temp_file, ".pdf"))
#' }
#' 
#' ## run interactively. Overrides default pdf options
#' if(interactive()){
#'     custom_create_PDF(user = TRUE, Labels = example_vector)
#' }
#' 
#' \dontrun{
#' ## run using a data frame, automatically choosing the "label" column
#' example_df <- data.frame("level1" = c("a1", "a2"), "label" = c("a1-b1",
#'  "a1-b2"), "level2" = c("b1", "b1"))
#' 
#' custom_create_PDF(user = FALSE, Labels = example_df, name = file.path(tempdir(), 
#'  "example_2"))
#'  }
#' \dontrun{
#' ## run using an unnamed data frame
#' example_df <- data.frame(c("a1", "a2"), c("a1-b1", "a1-b2"), c("b1", "b1"))
#' ## specify column from data frame
#' custom_create_PDF(user = FALSE, Labels = example_df[,2], name = file.path(tempdir(), "example_3"))
#' }
#' \dontrun{
#' ## create linear (code128) label rather than matrix (2D/QR) labels
#' example_df <- data.frame(c("a1", "a2"), c("a1-b1", "a1-b2"), c("b1", "b1"))
#' ## specify column from data frame
#' custom_create_PDF(user = FALSE, Labels = example_df, name = file.path(tempdir(),
#' "example_4", type = "linear"))
#' }
#' \dontrun{
#' ## Include text for the user that is NOT encoded into the barcode image
#' ## Excluded text is denoted with brackets by default
#' example_df <- data.frame(ID = floor(runif(3) * 10000), name = c("A", "B", "C"),
#'  dob = c("1/1/2020", "12/6/2001", "2/8/1986"))
#'  
#' ## linear (1d) barcodes with custom denote parameter
#' custom_create_PDF(Labels = example_df$ID, alt_text = paste(example_df$name,
#'  example_df$dob), type = "linear", denote=".")
#' }  
#' @seealso \code{\link{create_PDF}}
#' @export
#' @import qrcode

custom_create_PDF <- function(user = FALSE,
                              Labels = NULL,
                              name = "LabelsOut",
                              type = "matrix",
                              ErrCorr = "H",
                              Fsz = 12,
                              Across = TRUE,
                              ERows = 0,
                              ECols = 0,
                              trunc = TRUE,
                              numrow = 20,
                              numcol = 4,
                              page_width = 8.5,
                              page_height = 11,
                              width_margin = 0.25,
                              height_margin = 0.5,
                              label_width = NA,
                              label_height = NA,
                              x_space = 0,
                              y_space = 0.5,
                              alt_text = NULL,
                              replace_label = FALSE,
                              denote = c("\n(",")")

                              ){
  if (length(Labels) == 0) stop("Labels do not exist. Please pass in Labels")
  # what to do depending on class of Label input
  if(class(Labels)[1] %in% c("character", "integer", "numeric", "factor")){
    # treat as vector
    Labels <- Labels
  } else if (class(Labels)[1] == "data.frame") {
    if (any(tolower(names(Labels)) == "label")){
      Labels <- Labels[, "label"]
    } else {
      warning("Cannot find a label column. Using first column as label input.")
      Labels <- Labels[, 1]
    }
  } else {
    stop("Label input not a vector or a data frame. Please check your input.")
  }
  if (all(vapply(c(
    numcol, numrow, 
    Fsz, ERows, ECols, 
    trunc, page_width, page_height, 
    height_margin, width_margin, 
    x_space, y_space), is.numeric, logical(1L))) != TRUE) {
    stop("One or more numerical parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 1 | x_space < 0) stop("ERROR: x_space value out of bounds. Must be between 0 and 1")
  if (y_space < 0 | y_space > 1) stop("ERROR: y_space value out of bounds. Must be between 0 and 1")
  
  if (length(alt_text) > 0) {
    if(length(alt_text) != length(Labels)) {
      stop("Length of alt-text and Labels not equal.")
    }
    if (length(denote) == 1) {
      if (denote[1] %in% c("", NULL, NA, F)) {
        warning("alt-text is not encoded in barcode. Nothing shows the end-user which text is encoded. Use with caution! see ?custom_create_pdf")
      } else {
        warning(paste("alt_text is not encoded in barcode. Non-encoded text begins with",denote[1]))
        alt_text <- paste0(denote[1],alt_text)
      } 
    }
    if (length(denote) > 1) {
      warning(paste("alt_text is not encoded in barcode. Non-encoded text is denoted with",denote[1],denote[2]))
      alt_text <- paste0(denote[1],alt_text,denote[2])
    }
    alt_text <- as.factor(alt_text)
  }
  
  # clean up any open graphical devices if function fails
  on.exit(grDevices::graphics.off())
  # if user prompt has been set to true
  if (user == TRUE){
    
    ## ask for name
    name <- string_input("Please enter name for PDF output file: ")
    ## Set font size
    Fsz <- numeric_input("Please enter a font size: ", integer = FALSE)
    ## Error correction
    
    ErrCorr <- switch(
      fake_menu(
        c("L (up to 7% damage)", "M (up to 15% damage)", 
          "Q (up to 25% damage)", "H (up to 30% damage)"), 
        "Select an error correction level. "),
      "L", "M", "Q", "H")
    
    # check errCorr input
    Advanced <- switch(
      fake_menu(c("Yes", "No"),
                  "Edit advanced parameters?"), 
      TRUE, FALSE)
    
    if (Advanced) {
      type <- switch(
        fake_menu(c("Matrix QR Code", "Linear", "Linear2"),
                    "Type of Barcode: "),
        "matrix", "linear", "linear2"
      )
      
      ## Set to TRUE to print labels across rows instead of down columns
      Across <- switch(fake_menu(c("Yes", "No"), 
                                   "Print labels across?"), 
                       TRUE, FALSE)
      
      # Split text into rows (prevents text cutoff with narrow labels)
      trunc<- switch(fake_menu(c("Yes", "No"), 
                                 "Truncate longer labels into multiple lines if necessary?"), 
                     TRUE, FALSE)
      
      ERows <- numeric_input("Number of rows to skip? (enter 0 for default): ")
      
      ECols <- numeric_input("Number of cols to skip? (enter 0 for default): ")
      
      numrow <- numeric_input("Number of rows per page: ")
      
      numcol <- numeric_input("Number of col per page: ")
      
      height_margin <- numeric_input(
        "Please enter the height margin of page (in inches): ", integer = FALSE)
      
      width_margin <- numeric_input(
        "Please enter the width margin of page (in inches): ", integer = FALSE)
      
      label_width <- numeric_input(
        "Please enter the width of the label (in inches): ", integer = FALSE)
      
      label_height <- numeric_input(
        "Please enter the height of the label (in inches): ", integer = FALSE)
      
      if(type == "matrix"){
        space <- switch(
          numeric_input("Change distances between QR code and text label?\n 1: Yes \n 2: No"), 
          TRUE, FALSE)
      }
      
      x_space <- 0
      y_space <- 0.5
      if (space){
        x_space <- numeric_input("Please enter a number between 0 and 1 for \n 
                                 horizontal distance between QR code and label: ", 
                                 integer = FALSE)
        while(x_space > 1 ){
          noquote(print("Invalid input"))
          x_space <- numeric_input("Please enter a number between 0 and 1: ", integer = FALSE)
        }
        
        y_space <- numeric_input("Please enter a distance between 0 and 1 for \n
                                 vertical distance from bottom: ", integer = FALSE)
        
        while(y_space > 1){
          noquote(print("Invalid input"))
          numeric_input("Please enter a distance between 0 and 1: ", integer = FALSE)
        }
      }
    } ## end of advanced options loop
    
  }

  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  
  if(!is.numeric(label_width)){label_width <- width_margin/numcol}
  if(!is.numeric(label_height)){label_height <- height_margin/numrow}
  
  if((type == "linear" | type == "linear2") & label_width / labelLength < 0.03)
    warning("Linear barcodes created will have bar width smaller than 0.03 inches. \n  Increase label width to make them readable by all scanners.")
  
  column_space <- (width_margin - label_width * numcol)/(numcol - 1)
  row_space <- (height_margin - label_height * numrow)/(numrow - 1)
  
  # Viewport Setup
  ## grid for page, the layout is set up so last row and column do not include the spacers for the other columns
  barcode_layout <- grid::grid.layout(numrow, 
                                      numcol, 
                                      widths = grid::unit(c(rep(label_width + column_space, numcol-1), label_width), "in"),
                                      heights = grid::unit(c(rep(label_height + row_space, numrow-1), label_height), "in")
                                      )
  
  ## change viewport and barcode generator depending on qr or 1d barcodes
  if(type == "linear"){
    code_vp <- grid::viewport(x=grid::unit(0.05, "npc"), 
                              y=grid::unit(0.8, "npc"), 
                              width = grid::unit(0.9 *label_width, "in"), 
                              height = grid::unit(0.8 * label_height, "in"), 
                              just=c("left", "top"))
    
    text_height <- ifelse(Fsz / 72 > label_height * 0.3, label_height * 0.3, Fsz/72)
    
    label_vp <- grid::viewport(x=grid::unit(0.5, "npc"), 
                               y = grid::unit(1, "npc"), 
                               width = grid::unit(1, "npc"),
                               height = grid::unit(text_height, "in"), 
                               just = c("centre", "top"))

    Fsz <- ifelse(Fsz / 72 > label_height * 0.3, label_height * 72 * 0.3 , Fsz)

    label_plots <- sapply(as.character(Labels), code_128_make , USE.NAMES = TRUE, simplify = FALSE)
  } else if (type == "linear2"){
    code_vp <- grid::viewport(x=grid::unit(0.05, "npc"),
                              y=grid::unit(0.8, "npc"),
                              width = grid::unit(0.9 *label_width, "in"),
                              height = grid::unit(0.8 * label_height, "in"),
                              just=c("left", "top"))

    text_height <- ifelse(Fsz / 72 > label_height * 0.3, label_height * 0.3, Fsz/72)

    label_vp <- grid::viewport(x=grid::unit(0.5, "npc"),
                               y = grid::unit(1, "npc"),
                               width = grid::unit(1, "npc"),
                               height = grid::unit(text_height, "in"),
                               just = c("centre", "top"))

    Fsz <- ifelse(Fsz / 72 > label_height * 0.3, label_height * 72 * 0.3 , Fsz)

    label_plots <- sapply(as.character(Labels), code_128_make2 , USE.NAMES = TRUE, simplify = FALSE)
  } else if (type =="matrix"){
    ## vp for the qrcode within the grid layout
    code_vp <- grid::viewport(x=grid::unit(0.05, "npc"), 
                              y=grid::unit(0.8, "npc"), 
                              width = grid::unit(0.3 *label_width, "in"), 
                              height = grid::unit(0.6 * label_height, "in"), 
                              just=c("left", "top"))
    
    ## vp for the text label within the grid layout, scaling the x_space by 0.6 makes sure it will not overlap with the qrcode
    label_vp <- grid::viewport(x=grid::unit((0.4 + 0.6*x_space)*label_width, "in"), 
                               y=grid::unit(y_space, "npc"), 
                               width = grid::unit(0.4, "npc"), 
                               height = grid::unit(0.8, "npc"), 
                               just=c("left", "center"))
    
    # generate qr, most time intensive part
        label_plots <- sapply(as.character(Labels), qrcode_make, ErrCorr = ErrCorr, USE.NAMES = TRUE, simplify = FALSE)
  } else {stop("Barcode type must be linear, linear2 or matrix")}
  
  # since main text label is taken from label_plots, set names of vector
  # then null-out alt text
  if(replace_label){
    names(label_plots) <- alt_text
    alt_text <- NULL
  }
  # generate label positions
  
  if(Across){
    # across = TRUE
    positions <- expand.grid(x = 1:numcol, y = 1:numrow)
  } else {
    # across = FALSE
    positions <- expand.grid(y = 1:numrow, x = 1:numcol)
  }

  # make df of position for each label
  # this extra 5 is so that even if starting position is last cell, there are enough positions generated, hopefully
  duplication <- ceiling(length(Labels) / nrow(positions)) + 5
  
  label_positions <- do.call("rbind", replicate(duplication, positions, simplify = FALSE))
  
  # condition here for col/row skipping
  starting_pos_index <- min(which(label_positions$x == ECols + 1  & label_positions$y == ERows + 1))
  if(ECols > numcol | ERows > numrow){
      warning("Number of rows/columns to skip greater than number of rows/columns on page. Labels will start in top left corner.") 
      starting_pos_index <- 1
  }
  label_positions <- label_positions[seq(starting_pos_index, starting_pos_index + length(Labels)),]
  
  # File Creation

  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, 
                 width = page_width, 
                 height = page_height, 
                 onefile = TRUE, 
                 family = "Courier") # Standard North American 8.5 x 11
  
  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  
  for (i in seq(1,length(label_plots))){
    
    # Split label to count characters 
    Xsplt <- names(label_plots[i])
    Xalt <- paste(alt_text[i])
    lab_pos <- label_positions[i,]
    
    if(all(i != 1 & lab_pos == c(1, 1))){
      grid::grid.newpage()
      
      grid::pushViewport(
        grid::viewport(width = grid::unit(page_width, "in"), 
                       height = grid::unit(page_height, "in"))
      )
      # barcode_layout=grid.layout(numrow, numcol, widths = widths, heights = heights)
      grid::pushViewport(bc_vp)
    }
    
    if(trunc == TRUE){
      if(nchar(Xsplt) > 15){
        Xsplt <- paste0(substring(Xsplt, seq(1, nchar(Xsplt), 15), seq(15, nchar(Xsplt)+15-1, 15)), collapse = "\n")
      }
    }
    
    # Add alt_text
    if (length(alt_text) > 0) {
      Xsplt <- paste0(Xsplt,Xalt)
    }

    grid::pushViewport(grid::viewport(layout.pos.row=lab_pos$y, layout.pos.col=lab_pos$x))
    # grid::grid.rect()
    grid::pushViewport(code_vp)
    grid::grid.draw(label_plots[[i]])
    grid::popViewport()
    grid::pushViewport(label_vp)
    
    if(type =="linear" | type == "linear2"){
      grid::grid.rect(gp = grid::gpar(col = NA, fill = "white"))
    }
    
    grid::grid.text(label = Xsplt, gp = grid::gpar(fontsize = Fsz, lineheight = 0.8))
    
    grid::popViewport(2)
    
  }

} #end custom_create_PDF()

#' @rdname custom_create_PDF
#' @export

qrcode_make<-function(Labels, ErrCorr){
  # Create text label
  Xtxt<-gsub("_", "-", Labels)
  if(nchar(Xtxt) <= 1){
    Xtxt <- paste0("\\s\\s", Xtxt)
    warning("Label is single character or blank. Padding with empty spaces.")
  }
  # Create qrcode
  Xpng <- grid::rasterGrob(
    abs(qrcode::qr_code(paste0(Xtxt), 
                           ecl = ErrCorr) - 1), 
    interpolate = FALSE)
  return(Xpng)
}

#' @rdname custom_create_PDF
#' @export
code_128_make <- function(Labels){
  ## labels is a character string
  ## read in dict 
  Barcodes <- barcodes128
  ## double check Labels
  Labels <- as.character(Labels)
  Labels <- iconv(Labels, from = "utf-8", to = "ascii", sub = "-")
  start_code <- 209
  lab_chars <- unlist(strsplit(Labels, split = ""))
  lab_values <- sapply(lab_chars, function(x) utf8ToInt(x))
  # ascii to code 128 is just a difference of 32, this line keeps clarity
  code_values <- lab_values - 32
  # 104 is the start value for start code b, hardcoded right now
  check_sum <- 104 + sum(code_values * seq(1,length(code_values)))
  check_character <- check_sum %% 103
  Binary_code <- sapply(lab_values, 
                        function(x, Barcodes) Barcodes$Barcode[x == Barcodes$ASCII], 
                        Barcodes = Barcodes)
  ## create quiet zone
  quiet_zone <- paste(c(1:(10)*0),collapse="")
  ## paste together in order: quiet zone, start code binary, binary label, checksum character
  ## stop code, and quiet zone. Barcode for checksum is extracted based on position in Barcodes.
  binary_label <- paste(quiet_zone, 
                        Barcodes$Barcode[Barcodes$ASCII == start_code],
                        paste(Binary_code, collapse=""),
                        Barcodes$Barcode[check_character + 1],
                        "1100011101011",
                        quiet_zone,
                        collapse = "", sep ="")
  ## split binary apart for 
  bar_values <- as.numeric(unlist(strsplit(binary_label, split = "")))
  barcode_bars <- grid::rasterGrob(t(!as.matrix(bar_values)), width = 1, height = 1, interpolate = FALSE)
  return(barcode_bars)
}

#' @rdname custom_create_PDF
#' @export
code_128_make2 <- function(Labels) {
  ## labels is a character string
  ## read in dict
  Barcodes <- barcodes128
  ## double check Labels
  Labels <- enc2utf8(as.character(Labels))
  Labels <- iconv(Labels,
                  from = "utf-8",
                  to = "latin1",
                  sub = "-")
  # start_code <- 209
  lab_chars <- enc2utf8(unlist(strsplit(Labels, split = "")))

  is_number <- lab_chars %in% seq(0, 9)
  len1 <- length(lab_chars)
  tail <- seq(len1, 1)

  count_number <- 0
  count_not_number <- 0

  block_number <- rep(0L, len1)
  block_not_number <- rep(0L, len1)


  for (i in seq(len1, 1)) {
    if (is_number[i]) {
      count_number <- count_number + 1
      count_not_number <- 0L
    } else {
      count_number <- 0L
      count_not_number <- count_not_number + 1
    }
    block_number[i] <- count_number
    block_not_number[i] <- count_not_number
  }
  # print(rbind(lab_chars, is_number, block_number, block_not_number, tail))

  lab_values2 <- c()
  code_values2 <- c()
  last_code_B <- NULL

  i <- 1
  repeat {
    if (block_number[i] == 2L & len1 == 2L |
        block_number[i] > 3 & i == 1L |
        block_number[i] > 3 & tail[i] == block_number[i] |
        block_number[i] > 5) {

      res <- block_number[i] %% 2L
      pairs <- block_number[i] %/% 2L
      if (i==1L) {
        if(res == 1L) {
          # Start code 128B
          lab_values2 <- c(lab_values2, 209)
          code_values2 <- c(code_values2, 104)
          last_code_B <- TRUE
        } else {
          # Start code 128C
          lab_values2 <- c(lab_values2, 210)
          code_values2 <- c(code_values2, 105)
          last_code_B <- FALSE
        }
      }

      if (res == 1L) {

        utf_int1 <- utf8ToInt(lab_chars[[i]])

        if (31 < utf_int1 & utf_int1 < 160){

          lab_values2 <- c(lab_values2, utf_int1)
          code_values2 <- c(code_values2, utf_int1 - 32)

        } else {

          if (utf_int1 < 32) {
            # Shift A
            lab_values2 <- c(lab_values2, 203)
            code_values2 <- c(code_values2, 98)
            lab_values2 <- c(lab_values2, utf_int1 + 64 + 32)
            code_values2 <- c(code_values2, utf_int1 + 64)

          } else {
            # FNC4
            lab_values2 <- c(lab_values2, 205)
            code_values2 <- c(code_values2, 100)
            lab_values2 <- c(lab_values2, utf_int1 - 128)
            code_values2 <- c(code_values2, utf_int1 -128 - 32)
          }
        }

      }
      if (i > 1L | res == 1L) {
        # Code 128C
        lab_values2 <- c(lab_values2, 204)
        code_values2 <- c(code_values2, 99)
        last_code_B <- FALSE
      }

      for (j in seq(1, pairs)){
        pair1 <- as.integer(paste0(lab_chars[i+2*j-2+res],lab_chars[i+2*j-1+res]))
        lab_values2 <- c(lab_values2, pair1 + ifelse(pair1 < 95, 32, 105))
        code_values2 <- c(code_values2, pair1)

      }

    } else {
      if (i==1L) {
        # Start code 128B
        lab_values2 <- c(lab_values2, 209)
        code_values2 <- c(code_values2, 104)
        last_code_B <- TRUE
      } else {
        if (!last_code_B) {
          # Code 128B
          lab_values2 <- c(lab_values2, 205)
          code_values2 <- c(code_values2, 100)
          last_code_B <- TRUE
        }
      }
      for (j in seq(1, max(block_number[i],block_not_number[i]))){

        utf_int1 <- utf8ToInt(lab_chars[[i+j-1]])

        if (31 < utf_int1 & utf_int1 < 160){

          lab_values2 <- c(lab_values2, utf_int1)
          code_values2 <- c(code_values2, utf_int1 - 32)

        } else {

          if (utf_int1 < 32) {
            # Shift A
            lab_values2 <- c(lab_values2, 203)
            code_values2 <- c(code_values2, 98)
            lab_values2 <- c(lab_values2, utf_int1 + 64 + 32)
            code_values2 <- c(code_values2, utf_int1 + 64)

          } else {
            # FNC4
            lab_values2 <- c(lab_values2, 205)
            code_values2 <- c(code_values2, 100)
            lab_values2 <- c(lab_values2, utf_int1 - 128)
            code_values2 <- c(code_values2, utf_int1 -128 - 32)
          }
        }
      }
    }
    # print(i)
    i <- i + max(block_number[i],block_not_number[i])
    if (i > len1) {
      break
    }
  }

  # print(rbind(lab_values2, code_values2))


  # lab_values <- sapply(lab_chars, function(x) utf8ToInt(x))
  # ascii to code 128 is just a difference of 32, this line keeps clarity
  # code_values <- lab_values - 32
  # 104 is the start value for start code b, hardcoded right now
  # check_sum <- 104 + sum(code_values * seq(1, length(code_values)))
  check_sum <- sum(code_values2 * c(1,seq(1, length(code_values2)-1)))

  check_character <- check_sum %% 103
  Binary_code <- sapply(lab_values2,
                        function(x, Barcodes)
                          Barcodes$Barcode[x == Barcodes$ASCII],
                        Barcodes = Barcodes)
  ## create quiet zone
  quiet_zone <- paste(c(1:(10) * 0), collapse = "")
  ## paste together in order: quiet zone, start code binary, binary label, checksum character
  ## stop code, and quiet zone. Barcode for checksum is extracted based on position in Barcodes.
  binary_label <- paste(
    quiet_zone,
    #Barcodes$Barcode[Barcodes$ASCII == start_code],
    paste(Binary_code, collapse = ""),
    Barcodes$Barcode[check_character + 1],
    "1100011101011",
    quiet_zone,
    collapse = "",
    sep = ""
  )
  ## split binary apart for
  bar_values <-
    as.numeric(unlist(strsplit(binary_label, split = "")))
  barcode_bars <-
    grid::rasterGrob(
      t(!as.matrix(bar_values)),
      width = 1,
      height = 1,
      interpolate = FALSE
    )
  return(barcode_bars)
}


