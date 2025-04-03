
#' get_subdivision
#' 
#' Searches the division theme for a matching primary name.  Returns all
#' polygons (division_area) for which the matching primary name is listed
#' as the parent_division_id.  For instance, `get_subdivision("United States")`
#' returns a data frame containing the 50 states and 7 territories in the 
#' sovereign US., while `get_subdivision("California")` returns the counties
#' in California.
#' 
#' NOTE: This can be very slow without a very high-speed network connection.
#' 
#' @param primary_name the name listed as "primary" in Overture.
#' @return an `sf` object whose rows are all the polygon (or multipolygon)
#' features that are immediate children of the matching division.
#' 
#' @references https://docs.overturemaps.org/schema/reference/divisions/division/
#' @examplesIf interactive()
#' 
#' gdf <- get_subdivision("United States")
#' map(gdf)
#' 
#' gdf <- get_subdivision("California")
#' map(gdf)
#' 
#' @export
get_subdivision <- function(primary_name) {
  
  divisions <- overture("divisions", "division_area")
  division_ids <- overture("divisions", "division")
  
  division_ids |>
    dplyr::mutate(primary = struct_extract(names,"primary")) |>
    dplyr::filter(primary == {primary_name}) |>
    dplyr::select(parent_division_id = id) |>
    dplyr::inner_join(division_ids, by = "parent_division_id") |> 
    dplyr::select(division_id = id) |>
    dplyr::inner_join(divisions, by = "division_id") |>
    safe_gdf()
  
}

globalVariables(c("id", "primary", "struct_extract"))
