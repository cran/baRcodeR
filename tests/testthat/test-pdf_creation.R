context("Create PDF testing")


test_that("PDF successful generation", {
  # skip_on_cran()
  pdftemp <- tempfile(fileext = ".pdf")
  pdf_name <- tools::file_path_sans_ext(pdftemp)
  expect_invisible(
    custom_create_PDF(Labels = data.frame(label = c("example09", "example10"), ind_string = c("example", "example"), ind_number = c("09", "10")), name = pdf_name))
  expect_true(file.exists(pdftemp))
  expect_warning(
    custom_create_PDF(Labels = data.frame(not_label = c("example09", "example10"), ind_string = c("example", "example"), ind_number = c("09", "10")), name = pdf_name),
    "Cannot find a label column. Using first column as label input."
                 )
  expect_warning(
    custom_create_PDF(Labels = c(1, 2), type = "linear", label_width = 0.02, name = pdf_name),
                 "Linear barcodes created will have bar width smaller than 0.03 inches")
  expect_warning(
    custom_create_PDF(Labels = data.frame(label = c("example09", "example10"), ind_string = c("example", "example"), ind_number = c("09", "10")), numcol = 2, ECols = 3, name = pdf_name),
                 "Number of rows/columns to skip greater than number of rows/columns on page.")

})

test_that("PDF input errors", {
  expect_error(create_PDF(), "Labels do not exist. Please pass in Labels")
  expect_error(create_PDF(Labels = matrix(data=c(1:15), nrow=3, ncol = 5)), "Label input not a vector or a data frame")
  expect_error(custom_create_PDF(Labels = c(1, 2), x_space = 21), "ERROR: x_space value out of bounds.")
  expect_error(custom_create_PDF(Labels = c(1, 2), x_space = 251), "ERROR: x_space value out of bounds.")
  expect_error(custom_create_PDF(Labels = c(1, 2), y_space = 21), "ERROR: y_space value out of bounds.")
  expect_error(custom_create_PDF(Labels = c(1, 2), y_space = 251), "ERROR: y_space value out of bounds.")
  expect_error(custom_create_PDF(Labels = c(1, 2), y_space = "test"), "One or more numerical parameters are not numeric")
  expect_error(custom_create_PDF(Labels = data.frame(label = c("example09", "example10"), ind_string = c("example", "example"), ind_number = c("09", "10")), type="noformat"),
               "Barcode type must be linear or matrix")

})