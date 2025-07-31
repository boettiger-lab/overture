test_that("overture", {
  # duckdbfs::duckdb_secrets("", "", "s3.amazonaws.com")
  divisions <- overture("divisions", "division_area")
  expect_true(inherits(divisions, "tbl_duckdbfs"))
})

test_that("get_division", {
  ri <- get_division("Rhode Island", type = "region")
  expect_true(inherits(ri, "sf"))
  yolo <- get_division("Yolo County", as_sf = FALSE)
  expect_true(inherits(yolo, "tbl_duckdbfs"))
})
