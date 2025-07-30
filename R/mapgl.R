
## unpack "primary" from names struct

unpack_primary_name <- function(gdf) {
  if (!inherits(gdf, "tbl_duckdbfs")) {
    return(gdf)
  }
  dplyr::mutate(gdf, primary = struct_extract(names,"primary"))
}

# map fails with nested column types
drop_nested_cols <- function(gdf) {

  x <- gdf
  if (inherits(gdf, "tbl_lazy")) {
    x <- utils::head(gdf,1) |> dplyr::collect() # handle lazy tables
  }

  keep <- lapply(x, function(x) is.atomic(x) || inherits(x, "sfc"))
  cols <- names(keep[unlist(keep)])
  dplyr::select(gdf, dplyr::any_of(cols))

}

safe_gdf <- function(gdf) {
  if (inherits(gdf, "tbl_duckdbfs")) {
    gdf <- duckdbfs::to_sf(gdf, crs="epsg:4326")
  }
  gdf <- drop_nested_cols(gdf)

  gdf
}


#' to_s3
#' 
#' post as geojson in a public-readable s3 bucket for mapping purposes
#' 
#' Assumes AWS authentication / environmental variables are set.  
#' @param data gdf or duckdbfs table connection (e.g. from `overture()`)
#' @param hostpath bucket name and path to geojson
#' @return url to the geojson, e.g. for use with maplibre / mapgl
#' 
#' @export
to_s3 <- function(data, hostpath, id_col) {


  if (inherits(data, "tbl_lazy")) {
    dest <- paste0("s3://", hostpath)

    data |> 
      duckdbfs::as_dataset() |>
      duckdbfs::to_geojson(dest, id_col = id_col)
  } else if (inherits(data, "sf")) {
    dest <- paste0("/vsis3/", hostpath)
    sf::st_write(data, dest)
  }

  paste0("https://",
         Sys.getenv("AWS_S3_ENDPOINT"),
         "/",
         hostpath)
}


map <- function (gdf, 
                 hostpath = NULL,
                 tooltip = "primary",
                 fill_color = "blue",
                 fill_opacity = 0.1,
                 ...) {

  if (!requireNamespace("mapgl", quietly = TRUE)) {
    message("The mapgl package must be installed first.")
    return(invisible(NULL))
  }





  mapgl::maplibre() |>
    mapgl::add_source("gdf_source", gdf) |>
    mapgl::add_fill_layer(
      id = "gdf_layer",
      tooltip = tooltip,
      source = "gdf_source",
      fill_color = fill_color,
      fill_opacity = fill_opacity,
      ...
    ) # |> mapgl::fit_bounds(bounds)
  
}