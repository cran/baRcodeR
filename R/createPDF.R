#' Make barcodes and print labels
#'
#' Input vector or data.frame of ID codes to produce a PDF of QR codes which can
#' be printed. This is a wrapper function for \code{\link{custom_create_PDF}}.
#' See details of \code{\link{custom_create_PDF}} on how to format text labels
#' if needed.
#' 
#' The default PDF setup is for ULINE 1.75" * 0.5" WEATHER RESISTANT LABEL for laser
#' printer; item # S-19297 (uline.ca). The page format can be modified using
#' the \code{...} (advanced arguments) for other label types.
#'
#' @return a PDF file containing QR-coded labels, saved to the default directory.
#'
#'
#' @inheritParams custom_create_PDF
#' @param ... advanced arguments to modify the PDF layout. See
#'  \code{\link{custom_create_PDF}} for arguments. The advanced options can be
#'   accessed interactively with \code{user = TRUE} and then entering TRUE when prompted to
#'    modify advanced options.
#' @export
#' @examples
#' ## data frame
#' example_vector <- as.data.frame(c("ao1", "a02", "a03"))
#'
#' \dontrun{
#' ## run with default options
#' ## pdf file will be "example.pdf" saved into a temp directory
#' 
#' temp_file <- tempfile()
#' 
#' create_PDF(Labels = example_vector, name = temp_file)
#' 
#' ## view example output from temp folder
#' system2("open", paste0(temp_file, ".pdf"))
#' }
#' 
#' ## run interactively. Overrides default pdf options
#' if(interactive()){
#'     create_PDF(user = TRUE, Labels = example_vector)
#' }
#' 
#' \dontrun{
#' ## run using a data frame, automatically choosing the "label" column
#' example_df <- data.frame("level1" = c("a1", "a2"), "label" = c("a1-b1",
#' "a1-b2"), "level2" = c("b1", "b1"))
#' create_PDF(user = FALSE, Labels = example_df, name = file.path(tempdir(), "example_2"))
#' }
#' 
#' \dontrun{
#' ## run using an unnamed data frame
#' example_df <- data.frame(c("a1", "a2"), c("a1-b1", "a1-b2"), c("b1", "b1"))
#' ## specify column from data frame
#' create_PDF(user = FALSE, Labels = example_df[,2], name = file.path(tempdir(), "example_3"))
#' }
#' \dontrun{
#' ## create linear (code128) label rather than matrix (2D/QR) labels
#' example_df <- data.frame(c("a1", "a2"), c("a1-b1", "a1-b2"), c("b1", "b1"))
#' ## specify column from data frame
#' create_PDF(user = FALSE, Labels = example_df, name = file.path(tempdir(),
#' "example_4", type = "linear"))
#' }
#' @seealso \code{\link{custom_create_PDF}}


create_PDF <- function(user = FALSE,
                     Labels = NULL,
                     name ="LabelsOut",
                     type = "matrix",
                     ErrCorr = "H",
                     Fsz = 12, ...) {
  custom_create_PDF(user, Labels, name, type, ErrCorr, Fsz, ...)
}




