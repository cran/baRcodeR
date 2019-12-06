library(baRcodeR)

context("Input warnings")

test_that("input errors are produced", {
  expect_error(
    uniqID_maker(string="example", level="hi"), 
    "Level is not a string of numbers")
  
  expect_error(
    uniqID_maker(string="example", level = c(1:5), digits="a"), 
    "Digits is not a numerical value")
  
  expect_warning(
    uniqID_maker(string="example", level=1:300, digits=2), 
    "Digits specified less than max number. Increasing number of digits.")
  
  expect_warning(
    uniqID_maker(string="example", level=9:10, digits = 1), 
    "Digits specified less than max number. Increasing number of digits.")
 
  expect_error(
    uniqID_hier_maker(hierarchy = c("a",3,6)),
    "Hierarchy is not in list format")
  
   expect_error(
     uniqID_hier_maker(hierarchy = list(c("a",3,6), c("b",1))), 
     "Hierarchy entries are not of equal length.")
   
  expect_error(
    uniqID_hier_maker(hierarchy = list(c(3,6), c(1, 3))), 
    "Each level in hierarchy should have a string, a beginning value and an end value")
  
  expect_error(
    uniqID_hier_maker(hierarchy = list(c("a",3,6), c("b",1,"j"))), 
    "Invalid ending number on level")
  
  expect_error(
    uniqID_hier_maker(hierarchy = list(c("a",3,6), c("b","j",3))), 
    "Invalid starting number")
  
  expect_error(
    uniqID_hier_maker(hierarchy = list(c("a",3,6))), 
    "Input list has only one level")
  
  expect_warning(
    uniqID_hier_maker(hierarchy = list(c("a",3,6), c("b",100,103)), digits = 2),
    "Digits specified less than max level number")
  
  expect_warning(
    qrcode_make(1, "H"), 
    "Label is single character or blank.")
  
  temp_file <- tempfile()
  expect_error(
    custom_create_PDF(Labels = data.frame(label = c("ao1", "a02", "a03")), 
                      name = temp_file, 
                      numrow = "20"),
    "One or more numerical parameters are not numeric")
  expect_error(
    custom_create_PDF(
      Labels = data.frame(label = c("ao1", "a02", "a03")), 
      name = temp_file, 
      x_space = 1.5),
    "x_space value out of bounds. Must be between 0 and 1"
  )
  expect_error(
    custom_create_PDF(
      Labels = data.frame(label = c("ao1", "a02", "a03")), 
      name = temp_file, 
      y_space = 1.5),
    "y_space value out of bounds. Must be between 0 and 1"
  )

})

