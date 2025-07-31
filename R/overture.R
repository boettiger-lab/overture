release <- function() {
  getOption("overture-release", "2025-03-19.0")
}

bucket <- function() {
  getOption("overture-bucket", "overturemaps-us-west-2/release")
}

#' Access overture data
#'
#' Accesses overture data by theme and type.  This creates a streamable
#' connection to the cloud-hosted data using `duckdbfs`. It is possible
#' to use most `dplyr` commands to subset and manipulate this data,
#' see `dbplyr`.  Note that most spatial (PostGIS) operations are also
#' possible in this streaming mode, which avoids reading very large
#' data files into R.  Then use [duckdbfs::to_sf()] to read the resulting
#' subset into R as a native `sf` object.
#' @param theme overture theme
#' @param type data type (overture partition, not 'subtype')
#' @param release overture release string,
#'  see <https://docs.overturemaps.org/release/latest/>.
#'  This can also be configured in options().
#' @param bucket overture bucket path. Leave as default except to use local
#' mirrors, etc.  Also configurable using options().
#' @examplesIf interactive()
#'
#' divisions <- overture("divisions", "division_area")
#' divisions |> dplyr::filter(region == "US-CA")
#'
#' @export
overture <- function(
  theme = "divisions",
  type = "*",
  release = getOption("overture-release", "2025-07-23.0"),
  bucket = getOption("overture-bucket", "overturemaps-us-west-2")
) {
  if (bucket == "overturemaps-us-west-2") {
    duckdbfs::duckdb_secrets(
      key = "",
      secret = "",
      bucket = bucket,
      endpoint = "s3.amazonaws.com",
      region = "us-west-2"
    )
  }
  duckdbfs::load_spatial()

  ## FIXME consider support for configuring local storage

  path <- glue::glue(
    "s3://{bucket}/release/{release}/theme={theme}/type={type}/"
  )
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
  if ("geometry" %in% colnames(x)) {
    message("first 6 rows:")
    d <- utils::head(x)
    d <- duckdbfs::to_sf(d)
    x <- dplyr::as_tibble(d)
  }
  NextMethod(x)
}
