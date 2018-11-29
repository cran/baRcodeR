#' Make QR codes and print labels
#'
#' Input a vector or data frame of ID codes to produce a PDF of QR-coded labels 
#' that can then be printed. The PDF setup is for the ULINE 1.75" * 0.5"
#' WEATHER RESISTANT LABEL for laser printer; item # S-19297 (uline.ca)
#'
#' \code{barcode_make} is the helper function generating the actual QR code and
#' creating the page layout for printed labels.
#'
#' @return a PDF file containing QR-coded labels, saved to the default directory.
#'
#' @param user logical. Run function using interactive mode (prompts user for
#' parameter values) Default is \code{FALSE}
#' @param Labels vector or data frame object containing label names (i.e. unique ID codes).
#' @param name character. Name of the PDF output file. Default is \code{"LabelsOut"}.
#' @param ErrCorr error correction value. Level of damage from low to high:
#' \code{"L"}, \code{"M"}, \code{"Q"}, \code{"H"}. Default is \code{"H"}
#' @param Fsz numerical. Sets font size in points. Longer ID codes may be shrunk to fit if trunaction is not used. 
#' Default font size is \code{5}
#' @param Across logical. When \code{TRUE}, print labels across rows, left to right.
#' When \code{FALSE}, print labels down columns, top to bottom. Default is \code{TRUE}.
#' @param ERows number of rows to skip. Default is \code{0}. Example:
#' setting ERows to 6 will begin printing at row 7.
#' ERows and ECols are useful for printing on partially-used label sheets.
#' @param ECols number of columns to skip. Default is \code{0}. Example:
#' setting ECols to 2 will put the first label at column 3.
#' ERows and ECols are useful for printing on partially-used label sheets.
#' @param trunc logical. Text is broken into multiple lines for longer ID codes,
#' to prevent printing off of the label area. Default is \code{TRUE}. If \code{trunc = F}, 
#' and text is larger than the physical label, the text will be shrunk down automatically.
#' @param numrow numerical. Number of rows per page. Default is \code{20}.
#' @param numcol numerical. Number of columns per page. Default is \code{4}.
#' @param page_width numerical. Width of page (in inches). Default is set to \code{8.5}.
#' @param page_height numerical. Height of page (in inches). Default is set to \code{11}.
#' @param width_margin numerical. The width margin of the page (in inches).
#' Default is \code{0.25}.
#' @param height_margin numerical. The height margin of the page (in inches).
#' Default is \code{0.5}.
#' @param label_width numerical. The width of label (in inches). Will be calculated as
#' \code{(page_width - 2 * width_margin)/numcol} if \code{label_width} is set as \code{NULL}.
#' @param label_height numerical. The height of the label (in inches). Will be calculated as
#' \code{(page_height - 2 * height_margin)/numrow} if \code{label_height} is set as \code{NULL}.
#' @param x_space numerical. A value between \code{0} and \code{1}. This
#' sets the distance between the QR code and text of each label. Default is \code{0}.
#' @param y_space numerical. The height position of the text on the physical label as a 
#' proportion of the label height. A value between \code{0} and \code{1}.

#' @seealso \code{\link{create_PDF}}
#' @export
#' @import qrcode

custom_create_PDF <- function(user = FALSE,
                              Labels = NULL,
                              name = "LabelsOut",
                              ErrCorr = "H",
                              Fsz = 5,
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
                              y_space = 0.5
                              ){
  if (length(Labels) == 0) stop("Labels do not exist. Please pass in Labels")
  # what to do depending on class of Label input
  if(class(Labels) %in% c("character", "integer", "numeric", "factor")){
    # treat as vector
    Labels <- Labels
  } else if (class(Labels) == "data.frame") {
    if (any(tolower(names(Labels)) == "label")){
      Labels <- Labels[, "label"]
    } else {
      warning("Cannot find a label column. Using first column as label input.")
      Labels <- Labels[, 1]
    }
  } else {
    stop("Label input not a vector or a data frame. Please check your input.")
  }
  if (any(unlist(lapply(c(numcol, numrow, Fsz, ERows, ECols, trunc, page_width, page_height, height_margin, width_margin, x_space, y_space), class)) != "numeric") == TRUE) {
    stop("One or more numerical parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 1 | x_space < 0) stop("ERROR: x_space value out of bounds. Must be between 0 and 1")
  if (y_space < 0 | y_space > 1) stop("ERROR: y_space value out of bounds. Must be between 0 and 1")
  # clean up any open graphical devices if function fails
  on.exit(grDevices::dev.off())
  # if user prompt has been set to true
  if (user == TRUE){
    # possible inputs
    inputCheck <- c("T", "t", "F", "f")
    yesNo <- c("Y", "N")
    ## ask for name
    name <- readline(paste0("Please enter name for PDF output file: "))
    ## Set font size
    Fsz <- noquote(as.numeric(readline("Please enter a font size ")))
    while (Fsz <= 0){
      noquote(print("Invalid input, please specify a font size greater than 0"))
      Fsz <- noquote(as.numeric(readline("Please enter a font size greater than 0: ")))
    }
    ## Error correction
    ErrCorr <- noquote(toupper(readline("Specify an error correction - L, M, Q, H: ")))
    errCheck <- c("L", "l", "M", "m", "Q", "q", "H", "h")
    # check errCorr input
    while((ErrCorr %in% errCheck) == FALSE){
      noquote(print("Invalid input, please only enter what is specified"))
      ErrCorr <- noquote(toupper(readline("Specify an error correction - L, M, Q, H: ")))
    }
    Advanced <- noquote(toupper(readline("Edit advanced parameters? (Y/N): ")))
    while((Advanced %in% yesNo) == FALSE){
      noquote(print("Invalid input"))
      Advanced <- noquote(toupper(readline("Edit advanced parameters? (Y/N): ")))
    }
    if (Advanced =="Y") {
      ## Set to TRUE to print labels across rows instead of down columns
      Across <- noquote(toupper(readline("Please enter T or F to print across: ")))
      while((Across %in% inputCheck) == FALSE){
        noquote(print("Invalid input"))
        Across <- noquote(toupper(readline("Please enter T or F to print across: ")))
      }
      # Split text into rows (prevents text cutoff when label has >8 characters without \\n in labels)
      trunc<-noquote(toupper(readline("Do you want to split text into rows? (T/F): ")))
      while((trunc %in% inputCheck) == FALSE){
        noquote(print("Invalid input"))
        trunc <- noquote(toupper(readline("Do you want to split text into rows? (T/F): ")))
      }
      ERows <- noquote(as.numeric(readline("Number of rows to skip? (enter 0 for default): ")))
      ECols <- noquote(as.numeric(readline("Number of cols to skip? (enter 0 for default): ")))
      numrow <- as.numeric(readline("# of rows per page: "))
      while(is.numeric(numrow) == FALSE) {
        noquote(print("Invalid input"))
        numrow <- as.numeric(readline("# of rows per page: "))
      }
      numcol <- as.numeric(readline("# of col per page: "))
      while(is.numeric(numcol) == FALSE) {
        noquote(print("Invalid input"))
        numcol <- as.numeric(readline("# of col per page: "))
      }
      height_margin <- as.numeric(readline("Please enter the height margin of page (in inch): "))
      while(is.numeric(height_margin) == FALSE){
        noquote(print("Invalid input"))
        height_margin <- as.numeric(readline("Please enter the height margin of page (in inch): "))
      }
      width_margin <- as.numeric(readline("Please enter the width margin of page (in inch): "))
      while(is.numeric(width_margin) == FALSE){
        noquote(print("Invalid input"))
        width_margin <- as.numeric(readline("Please enter the width margin of page (in inch): "))
      }
      label_width <- as.numeric(readline("Please enter the width of the label (in inch): "))
      while(is.numeric(label_width) == FALSE){
        noquote(print("Invalid input"))
        label_width <- as.numeric(readline("Please enter the width of the label (in inch): "))
      }
      label_height <- as.numeric(readline("Please enter the height of the label (in inch): "))
      while(is.numeric(label_height) == FALSE){
        noquote(print("Invalid input"))
        label_height <- as.numeric(readline("Please enter the height of the label (in inch): "))
      }
      space <- toupper(readline("change distance between qrcode and label? (y/n): "))
      while((space %in% yesNo) == FALSE){
        noquote(print("Invalid input"))
        space<-toupper(readline("change distance between qrcode and label? (y/n): "))
      }
      x_space <- 0
      y_space <- 0.5
      if (space=="Y"){
        x_space <- as.numeric(readline("Please enter a distance between 0 and 1: "))
        while((x_space < 1 | x_space > 0)){
          noquote(print("Invalid input"))
          x_space <- as.numeric(readline("Please enter a distance between 0 and 1: "))
        }
        y_space <- as.numeric(readline("Please enter a value between 0 and 1: "))
        while((x_space < 0 | x_space > 1)){
          noquote(print("Invalid input"))
          y_space <- as.numeric(readline("Please enter a distance between 0 and 1:"))
        }
      }
    } ## end of advanced options loop


  } # user ask == T
  # Dummy data.frame for plotting

  # if (Fsz >= 2.2 && Fsz <= 2.5 && labelLength >= 27) stop("ERROR: not enought space to print full label, please decrease font size")

  width_margin <- page_width - width_margin * 2
  height_margin <- page_height - height_margin * 2
  if(is.na(label_width)){label_width <- width_margin/numcol}
  if(is.na(label_height)){label_height <- height_margin/numrow}
  if(!is.numeric(c(label_width, label_height))) stop("label_width and label_height should be set to NULL or a numeric value.")
  # if (cust_spacing == T) {
  #   y_space <- x_space - (as.integer(x_space * 0.5)) - 15
  # } else {
  #   y_space <- 182
  # }
  column_space <- (width_margin - label_width * numcol)/(numcol-1)
  row_space <- (height_margin - label_height * numrow)/(numrow-1)
  # Viewport Setup
  ## grid for page, the layout is set up so last row and column do not include the spacers for the other columns
  barcode_layout <- grid::grid.layout(numrow, numcol, widths = grid::unit(c(rep(label_width + column_space, numcol-1), label_width), "in"), heights = grid::unit(c(rep(label_height + row_space, numrow-1), label_height), "in"))
  ## vp for the qrcode within the grid layout
  qr_vp <- grid::viewport(x=grid::unit(0.05, "npc"), y=grid::unit(0.8, "npc"), width = grid::unit(0.3 *label_width, "in"), height = grid::unit(0.6 * label_height, "in"), just=c("left", "top"))
  ## vp for the text label within the grid layout, scaling the x_space by 0.6 makes sure it will not overlap with the qrcode
  label_vp <- grid::viewport(x=grid::unit((0.4 + 0.6*x_space)*label_width, "in"), y=grid::unit(y_space, "npc"), width = grid::unit(0.4, "npc"), height = grid::unit(0.8, "npc"), just=c("left", "center"))
  # generate qr, most time intensive part
  label_plots <- sapply(as.character(Labels), qrcode_make, ErrCorr = ErrCorr, USE.NAMES = T, simplify = F)
  # File Creation
  x_pos <- ERows + 1
  y_pos <- ECols + 1
  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, width = page_width, height = page_height, onefile = TRUE, family = "Courier") # Standard North American 8.5 x 11
  bc_vp = grid::viewport(layout = barcode_layout)
  grid::pushViewport(bc_vp)
  
  for (i in 1:length(label_plots)){
    # Split label to count characters
    Xsplt <- names(label_plots[i])
    if(trunc == TRUE){  # Truncate string across lines if trunc==T
      if(nchar(Xsplt) > 27){Xsplt <- Xsplt[1:27]}
      # If remaining string is > 8 characters, split into separate lines
      if(nchar(Xsplt) > 12){
        Xsplt <- paste0(substring(Xsplt, seq(1, nchar(Xsplt), 12), seq(12, nchar(Xsplt)+12-1, 12)), collapse = "\n")
      }
    }
    # print(c("in", x_pos, y_pos))
    # reset if any of the values are greater than page limits
    if (x_pos > numcol | y_pos > numrow){
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(width = grid::unit(page_width, "in"), height = grid::unit(page_height, "in")))
      # barcode_layout=grid.layout(numrow, numcol, widths = widths, heights = heights)
      grid::pushViewport(bc_vp)
      x_pos = 1
      y_pos = 1
    }
    #print(c(x_pos, y_pos))
    # print the label onto the viewport
    grid::pushViewport(grid::viewport(layout.pos.row=y_pos, layout.pos.col=x_pos))
    # grid.rect()
    grid::pushViewport(qr_vp)
    grid::grid.draw(label_plots[[i]])
    grid::popViewport()
    grid::pushViewport(label_vp)
    grid::grid.text(label = Xsplt, gp = grid::gpar(fontsize = Fsz, lineheight = 0.8))
    grid::popViewport(2)
    if (Across == "T" | Across == TRUE){
      x_pos <- x_pos + 1
      if (x_pos > numcol) {
        x_pos <- 1
        y_pos <- y_pos + 1
      }
      
    } else {
      y_pos <- y_pos + 1
      if (y_pos > numrow) {
        y_pos <- 1
        x_pos <- x_pos + 1
      }
    }
    # print(c("out", x_pos, y_pos))
  }


  #end if
} #end create_PDF()


#' @rdname  custom_create_PDF
#' @export
qrcode_make<-function(Labels, ErrCorr){
  # Create text label
  Xtxt<-gsub("_", "-", Labels)
  # Create qrcode
  Xpng <- grid::rasterGrob(abs(qrcode::qrcode_gen(paste0(Xtxt), ErrorCorrectionLevel = ErrCorr, dataOutput = TRUE, plotQRcode = FALSE, mask = 3) - 1), interpolate = FALSE)
  return(Xpng)
}