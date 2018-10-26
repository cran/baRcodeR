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
#' @param Fsz numerical. Sets font size using a number between \code{2.2} and
#'  \code{4.7}. Longer ID codes may not fit on the labels using larger font sizes. 
#' Default font size is \code{2.5}
#' @param Across logical. When \code{TRUE}, print labels across rows, left to right.
#' When \code{FALSE}, print labels down columns, top to bottom. Default is \code{TRUE}.
#' @param ERows number of rows to skip. Default is \code{0}. Example:
#' setting ERows to 6 will begin printing at row 7.
#' ERows and ECols are useful for printing on partially-used label sheets.
#' @param ECols number of columns to skip. Default is \code{0}. Example:
#' setting ECols to 2 will put the first label at column 3.
#' ERows and ECols are useful for printing on partially-used label sheets.
#' @param trunc logical. Text is broken into multiple lines for longer ID codes,
#' to prevent printing off of the label area. Default is \code{TRUE}.
#' @param numrow numerical. Number of rows per page. Default is \code{20}.
#' @param numcol numerical. Number of columns per page. Default is \code{4}.
#' @param page_width numerical. Width of page (in inches). Default is set to \code{8.5}.
#' @param page_height numerical. Height of page (in inches). Default is set to \code{11}.
#' @param height_margin numerical. The height margin of the page (in inches).
#' Default is \code{0.5}.
#' @param width_margin numerical. The width margin of the page (in inches).
#' Default is \code{0.25}.
#' @param x_space numerical. An integer between \code{190} - \code{250}. This
#' sets the distance between the QR code and text of each label. Default is \code{215}.
#' @param y_space numerical. An integer between 80 and 215. Default is 182.

#' @seealso \code{\link{create_PDF}}
#' @export
#' @import qrcode

custom_create_PDF <- function(user = FALSE,
                              Labels = NULL,
                              name = "LabelsOut",
                              ErrCorr = "H",
                              Fsz = 2.5,
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
                              x_space = 215,
                              y_space = 182
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
    stop("One or more numerica parameters are not numeric")
  }
  labelLength <- max(nchar(paste(Labels)))
  if (x_space > 250 | x_space < 190) stop("ERROR: x_space value out of bounds. Must be between 190 - 250")
  if (y_space < 80 | y_space > 215) stop("ERROR: y_space value out of bounds. Must be between 80 - 215")
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
    Fsz <- noquote(as.numeric(readline("Please enter a font size (2.2-4.7): ")))
    while (Fsz < 2.2 | Fsz > 4.7){
      noquote(print("Invalid input, please specify a font size within the range 2.2-4.7"))
      Fsz <- noquote(as.numeric(readline("Please enter a font size (2.2-4.7): ")))
    }

    while (Fsz >= 2.2 && Fsz <= 2.5 && labelLength >= 27){
      noquote(print("ERROR: not enought space to print full label, please decrease font size"))
      Fsz <- noquote(as.numeric(readline("Please enter a font size (2.2-4.7): ")))
    }
    while (Fsz >= 2.6 && Fsz <= 4.0 && labelLength >= 18){
      noquote(print("ERROR: not enought space to print full label, please decrease font size"))
      Fsz <- noquote(as.numeric(readline("Please enter a font size (2.2-4.7): ")))
    }
    while(Fsz >= 4.1 && Fsz <= 4.7 && labelLength >= 9){
      noquote(print("ERROR: not enought space to print full label, please decrease font size"))
      Fsz <- noquote(as.numeric(readline("Please enter a font size (2.2-4.7): ")))
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
      space <- toupper(readline("change distance between qrcode and label? (y/n): "))
      while((space %in% yesNo) == FALSE){
        noquote(print("Invalid input"))
        space<-toupper(readline("change distance between qrcode and label? (y/n): "))
      }
      x_space <- 215
      y_space <- 92.5
      if (space=="Y"){
        x_space <- as.numeric(readline("Please enter a distance between 190-250: "))
        while((x_space < 190 | x_space > 250)){
          noquote(print("Invalid input"))
          x_space <- as.numeric(readline("Please enter a distance between 190-250: "))
        }
        y_space <- as.numeric(readline("Please enter a distance between 80-215: "))
        while((x_space < 80 | x_space > 215)){
          noquote(print("Invalid input"))
          y_space <- as.numeric(readline("Please enter a distance between 80-215: "))
        }
      }
    } ## end of advanced options loop


  } # user ask == T
  # Dummy data.frame for plotting

  if (Fsz >= 2.2 && Fsz <= 2.5 && labelLength >= 27) stop("ERROR: not enought space to print full label, please decrease font size")
  width_margin <- 8.5 - width_margin * 2
  height_margin <- 11 - height_margin * 2
  # if (cust_spacing == T) {
  #   y_space <- x_space - (as.integer(x_space * 0.5)) - 15
  # } else {
  #   y_space <- 182
  # }
  ### Page Setup
  label_plots <- lapply(Labels, barcode_make, trunc = trunc, ErrCorr = ErrCorr, x_space = x_space, y_space = y_space, Fsz = Fsz)
  
  x_pos <- ERows + 1
  y_pos <- ECols + 1
  
  oname <- paste0(name, ".pdf")
  grDevices::pdf(oname, width = page_width, height = page_height, onefile = TRUE, family = "Courier") # Standard North American 8.5 x 11
  grid::grid.newpage() # Open a new page on grid device
  grid::pushViewport(grid::viewport(width = grid::unit(width_margin, "in"), height = grid::unit(height_margin, "in"), just = c("centre", "centre"), layout = grid::grid.layout(numrow, numcol))) # Margins: left/right:10mm x top/bottom:22mm

  for (i in 1:length(label_plots)){
    # print(c("in", x_pos, y_pos))
    # reset if any of the values are greater than page limits
    if (x_pos > numrow | y_pos > numcol){
      grid::grid.newpage() # Open a new page on grid device
      grid::pushViewport(grid::viewport(width = grid::unit(width_margin, "in"), height = grid::unit(height_margin, "in"), just = c("centre","centre"), layout = grid::grid.layout(numrow, numcol))) # Margins: left/right:10mm x top/bottom:22mm
      x_pos = 1
      y_pos = 1
    }
    #print(c(x_pos, y_pos))
    # print the label onto the viewport
    print(label_plots[[i]], vp = grid::viewport(layout.pos.row = x_pos, layout.pos.col = y_pos, x = grid::unit(0,"mm"), y = grid::unit(0,"mm"), clip = FALSE))
    if (Across == "T" | Across == TRUE){
      y_pos <- y_pos + 1
      if (y_pos > numcol) {
        y_pos <- 1
        x_pos <- x_pos + 1
      }
    } else {
      x_pos <- x_pos + 1
      if (x_pos > numrow) {
        x_pos <- 1
        y_pos <- y_pos + 1
      }
    }
    # print(c("out", x_pos, y_pos))
  }

  #end if
} #end create_PDF()


#' @rdname custom_create_PDF
#' @export
barcode_make<-function(Labels, trunc, ErrCorr, x_space, y_space, Fsz){
  dummy_df <- data.frame(x = c(0, 457), y = c(0, 212))
  # Create text label
  Xtxt<-paste(gsub("\\\\n", "\\\n", Labels), collapse="")
  # Split label to count characters
  Xsplt <- strsplit(Xtxt, "")[[1]]

  if(trunc == TRUE){  # Truncate string across lines if trunc==T
    if(length(Xsplt) > 27){Xsplt <- Xsplt[1:27]}
    # If remaining string is > 8 characters, split into separate lines
    if(length(Xsplt) > 8){
      Xnew <- {}
      count <- 0
      for(j in 1:length(Xsplt)){
        count <- count + 1
        Xnew <- c(Xnew, Xsplt[j])
        if(count > 8){
          count<- 0
          Xnew <- c(Xnew,"\n")
        }
      }
      Xtxt <- paste(Xnew, collapse="")
    }
  }
  # Create qrcode
  Xpng <- grid::rasterGrob(abs(qrcode::qrcode_gen(paste0(Labels), ErrorCorrectionLevel = ErrCorr, dataOutput = TRUE, plotQRcode = FALSE, mask = 3) - 1), interpolate = FALSE)
  # Create tag (QR code + text label)
  Xplt <-
    ggplot2::ggplot(data = dummy_df, ggplot2::aes(x = 0, y = 0)) + ggplot2::theme_void() + ggplot2::annotation_custom(Xpng, xmin = 30, xmax = 180, ymin = 60, ymax = 180) + ggplot2::coord_cartesian(xlim = c(0, 457), ylim = c(0, 212)) + 
    ggplot2::geom_text(ggplot2::aes(x = x_space, y = y_space, label = Xtxt, hjust = 0, vjust = 1), size = Fsz)
  return(Xplt)
}

