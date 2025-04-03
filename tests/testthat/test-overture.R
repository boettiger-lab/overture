
test_that("overture", {
  
  duckdbfs::duckdb_secrets("", "", "s3.amazonaws.com")
  divisions <- overture("divisions", "division_area")
  expect_true(inherits(divisions, "tbl_duckdbfs"))
  
  
})