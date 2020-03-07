context("Checking custom input functions")

test_that("numeric input works", {
  f <- file()
  options(baRcodeR.connection = f)
  ans <- paste(c("a", "a", "a",
                 "3", 3, 3.5, 3.5), collapse = "\n") 
  write(ans, f)
  
  expect_error(numeric_input("Testing ... : "), "Invalid input. Please try again")
  expect_equal(numeric_input("Testing ...: "), 3)
  expect_equal(numeric_input("Testing ...: "), 3)
  expect_equal(numeric_input("Testing ...: "), 3)
  expect_equal(numeric_input("Testing ...: ", integer = FALSE), 3.5)
  
  options(baRcodeR.connection = stdin())
  close(f)
})


test_that("fake menu works", {
  f <- file()
  options(baRcodeR.connection = f)
  ans <- paste(c(1, 2, 4, 0, 4), collapse = "\n") 
  write(ans, f)
  
  expect_equal(fake_menu(c("Yes", "No"), 
                         "Menu testing ... "), 1)
  expect_equal(fake_menu(c("Yes", "No"), 
                         "Menu testing ... "), 2)
  expect_error(fake_menu(c("Yes", "No"), 
                         "Menu testing ... "), "Invalid input. Please try again.")
  
  options(baRcodeR.connection = stdin())
  close(f)  
})


context("Interactive with uniqID-maker")


test_that("interactive for sequential works properly", {
  # this only tests the steps in the user prompts and the prompt specific errors
  # set up mock user input
  f <- file()
  options(baRcodeR.connection = f)
  ans <- paste(c("example", "1", "2", "3", 
                 "example", "1", "2", "1",
                 "example", "a", "a", "a"), collapse = "\n") 
  write(ans, f)
  
  expect_identical(
    uniqID_maker(user = T), 
    data.frame(label = c("example001", "example002"), 
               ind_string = c("example", "example"), 
               ind_number = c("001", "002"), stringsAsFactors = FALSE))
  
  expect_identical(
    uniqID_maker(user = T), 
    data.frame(label = c("example1", "example2"), 
               ind_string = c("example", "example"), 
               ind_number = c("1", "2"), stringsAsFactors = FALSE))
  
  expect_error(
    uniqID_maker(user = T),
    "Invalid input. Please try again."
  )
  
  options(baRcodeR.connection = stdin())
  close(f)
  
})




context("Interactive with uniqID-hier")

test_that("hierarchical user prompts work correctly", {
  g <- file()
  options(baRcodeR.connection = g)
  ans <- paste(c(2, 2, 2,
                 "a", "4", "5", 
                 "b", 1, 2), collapse = "\n") 
  write(ans, g)
  
  example_hier_df <- data.frame(label = c("a04-b01", "a04-b02", "a05-b01", "a05-b02"), a = c("a04", "a04", "a05", "a05"), b = c("b01", "b02", "b01", "b02"), stringsAsFactors = FALSE)
  example_hier_df$label <- as.character(example_hier_df$label)
  expect_equal(
    uniqID_hier_maker(user = T),
    example_hier_df
  )
  
  options(baRcodeR.connection = stdin())
  close(g)
  
})


context("Interactive with create PDF")

test_that("PDF creation prompts work correctly", {
  # skip_on_cran()
  pdftemp <- tempfile(fileext = ".pdf")
  pdf_name <- tools::file_path_sans_ext(pdftemp)
  
  f <- file()
  options(baRcodeR.connection = f)
  ans <- paste(c(pdf_name, 12, 4,
                 1, 1, 1, 
                 1, 0, 0,
                 20, 4, 0.5, 0.25, 
                 2, 0.5, 2, collapse = "\n"))
  write(ans, f)
  
  custom_create_PDF(user = T, Labels = data.frame(label = c("example09", "example10"), ind_string = c("example", "example"), ind_number = c("09", "10")))
  
  expect_true(file.exists(pdftemp))
  options(baRcodeR.connection = stdin())
  close(f)
  
})
