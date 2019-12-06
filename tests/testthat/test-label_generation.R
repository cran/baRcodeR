context("Label Generation")

test_that("labels are generated properly", {
  expect_identical(
    uniqID_maker(string="example", level=1:2), 
    data.frame(label = c("example001", "example002"), ind_string = c("example", "example"), ind_number = c("001", "002")))
  expect_identical(
    uniqID_maker(string="example", level=c(3,1)), 
    data.frame(label = c("example003", "example001"), ind_string = c("example", "example"), ind_number = c("003", "001")))
  expect_identical(
    uniqID_maker(string = "example", level = 1:2, ending_string = "end"),
    data.frame(label = c("example001-end", "example002-end"), ind_string = c("example", "example"), ind_number = c("001", "002"), end_string = c("end", "end"))
  )
  expect_identical(
    uniqID_maker(string="example", level=1:2, digits = 1), 
    data.frame(label = c("example1", "example2"), ind_string = c("example", "example"), ind_number = c("1", "2")))
  example_hier_df <- data.frame(label = c("a04-b01", "a04-b02", "a05-b01", "a05-b02"), a = c("a04", "a04", "a05", "a05"), b = c("b01", "b02", "b01", "b02"))
  example_hier_df$label <- as.character(example_hier_df$label)
    expect_equal(
    uniqID_hier_maker(hierarchy = list(c("a",4,5), c("b", 1, 2))),
    example_hier_df
                   )
})

test_that("are rasters generated?", {
  expect_s3_class(code_128_make("test"), "rastergrob")
  expect_s3_class(qrcode_make("test", "H"), "rastergrob")
})