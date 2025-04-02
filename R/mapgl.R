
## unpack "primary" from names struct

unpack_primary_name <- function(gdf) {
  
  if (!inherits(gdf, "tbl_duckdbfs") ) {
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
    gdf <- unpack_primary_name(gdf)
    gdf <- duckdbfs::to_sf(gdf, crs="epsg:4326") 
  }
  gdf <- drop_nested_cols(gdf)
  
  gdf
}

map <- function (gdf, 
                 tooltip = "primary", 
                 fill_color = "blue", 
                 fill_opacity = 0.1, ...) {

  if (!require(mapgl, quietly = TRUE)) {
    message("The mapgl package must be installed first.")
    return(invisible(NULL))
    
  }
  
  mapgl::maplibre(bounds = gdf) |>
    mapgl::add_fill_layer(
    id = "gdf_data",
    tooltip = tooltip,
    source = safe_gdf(gdf),
    fill_color = fill_color,
    fill_opacity = fill_opacity,
    ...
  )
  
}