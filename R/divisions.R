


subtype = c(
'country',
'dependency',
'region',
'county',
'localadmin',
'locality',
'macrohood',
'neighborhood',
'microhood')

division <- function(geom_type = c("polygon", "point", "line")) {
  geom_type <- match.arg(geom_type)
  division_geom_type <- switch(geom_type, 
                               polygon = "division_area",
                               point = "division",
                               line = "division_boundary",
                               "division_area")
  
  overture("divisions", division_geom_type)
}

#' get_subdivision("United States")
get_subdivision <- function(primary_name, country = NULL) {
  
  divisions <- division("polygon")
  division_ids <- division("point")
  
  if(!is.null(country)) {
    division_ids <- filter(division_ids, country == {country})
    divisions <-filter(divisions, country == {country})
  }
  
  division_ids |>
    mutate(primary = struct_extract(names,"primary")) |>
    filter(primary == {primary_name}) |>
    select(parent_division_id = id) |>
    inner_join(division_ids, by = "parent_division_id") |> 
    select(division_id = id) |>
    inner_join(divisions, by = "division_id") |>
    safe_gdf()
  
}




# US "States" are Regions, regions are encoded as US-CA
# counties do not have a filter