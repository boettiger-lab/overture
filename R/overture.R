

releases <- function() {
  "2025-03-19.0"
}

bucket <- function() {
  "overturemaps-us-west-2"
}

#' Access overture data
#' 
#' @param theme overture theme
#' @param type data type (overture partition, not 'subtype')
#' @examplesIf interactive()
#' overture()
#' 
#' # example code
#' 
#' @export
overture <- function(theme="divisions", type = "division_area") {
  duckdbfs::duckdb_secrets("", "", "s3.amazonaws.com")
  duckdbfs::load_spatial()
  
  release <- releases()
  bucket <- bucket()
  
  path <- glue::glue("s3://{bucket}/release/{release}/theme={theme}/type={type}/")
  d <- duckdbfs::open_dataset(path)
  # dplyr::mutate(d, geometry = ST_GeomFromWKB(geometry))
  classes <- class(d)
  class(d) <- c("tbl_duckdbfs", classes)
  
  d
}


#duckdbfs::duckdb_secrets("", "", "s3.amazonaws.com")
#duckdbfs::open_dataset('s3://overturemaps-us-west-2/release/2025-03-19.0/theme=divisions/type=division_area/**')

#' @export
print.tbl_duckdbfs <- function(x, ...) {
  
  if("geometry" %in% colnames(x)) {
    message("first 6 rows:")
    d <- utils::head(x)
    d <- duckdbfs::to_sf(d)
    x <- dplyr::as_tibble(d)
  }
  NextMethod(x)
}