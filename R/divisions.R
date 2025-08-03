overture_cache <- function() {
  tools::R_user_dir("overture", "cache")
}

cache <- function() {
  dir <- overture_cache()
  dir.create(dir, FALSE, TRUE) # ensure dir exists
  memoise::cache_filesystem(dir)
}


#' @importFrom rlang !!
get_division_ <- function(
  primary_name,
  type = NULL,
  as_sf = TRUE,
  is_land = TRUE,
  country = NULL,
  region = NULL
) {
  gdf <- overture("divisions", "division_area")

  if (!is.null(type)) {
    gdf <- gdf |> dplyr::filter(subtype %in% !!type)
  }
  if (!is.null(is_land)) {
    gdf <- gdf |> dplyr::filter(is_land == !!is_land)
  }

  if (!is.null(country)) {
    gdf <- gdf |> dplyr::filter(country == !!country)
  }

  if (!is.null(region)) {
    gdf <- gdf |> dplyr::filter(region == !!region)
  }

  gdf <- gdf |>
    dplyr::mutate(primary = struct_extract(names, "primary")) |>
    dplyr::filter(primary == !!primary_name)

  # Always return only largest area
  gdf <- gdf |>
    dplyr::mutate(area = st_area(geometry)) |>
    dplyr::arrange(desc(area)) |>
    utils::head(1)

  if (as_sf) {
    gdf <- safe_gdf(gdf)
  }

  gdf
}


#' get_division
#'
#' Return a polygon for the desired division
#' (e.g. country, region, county, etc).
#' By default will search for a direct match to the Overture primary name across
#' all Overture subtypes.  Specify the type for faster search and/or to resolve
#' ambiguities.
#'
#' In the case of multiple matches, get_division() will only return the largest
#' match by area.  If other matches are required, specify
#'
#' @param primary_name the primary name of the division, e.g. "United States",
#'    or "California".
#' @param type the Overture subtype, e.g. "country", "region", "county", "locality",
#'  "neighborhood" etc. By default will search all types.
#' @param as_sf Return an `sf` object?  default `TRUE`. Set to FALSE only to continue
#' additional processing outside-of-RAM with duckdbfs spatial tools.
#' @param is_land logical, default TRUE. Avoids returning maritime regions unexpectedly.
#' Set to NULL to return all values.
#' @param country An optional 2-letter country code, can be used to filter results to avoid ambiguities.
#' @param region a region code, e.g. US-CA (California).
#' @return An `sf` object or corresponding lazy_tbl of the matching data
#' @export
#'
#' @examplesIf  interactive()
#'
#' uk <- get_division("United Kingdom")
#' ca <- get_division("California", type = "region")
#' library(sf)
#' plot(uk[1])
get_division <- memoise::memoise(get_division_, cache = cache())


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

  gdf <- division_ids |>
    dplyr::mutate(primary = struct_extract(names, "primary")) |>
    dplyr::filter(
      primary ==
        {
          primary_name
        }
    ) |>
    dplyr::select(parent_division_id = id) |>
    dplyr::inner_join(division_ids, by = "parent_division_id") |> # Filter ids to children of the matched primary_name
    dplyr::select(division_id = id) |>
    dplyr::inner_join(divisions, by = "division_id") |> # Filter to geometries of the matched polygons
    dplyr::mutate(primary = struct_extract(names, "primary"))

  gdf
}


dummy_fn <- function() {
  # BECAUSE R CHECKS ARE SOMETIMES RIDICULUOUS

  # digest is an optional import for memoise but is needed our use of memoise.
  # dbplyr is an optional import for dbplyr but needed in our use of dplyr.

  # We have to list these packages in DEPENDS to avoid errors.
  # But cannot have a package in DEPENDS without an explicit call to it.

  digest::digest("1")
  dbplyr::as.sql("COPY")
}

globalVariables(c(
  "id",
  "primary",
  "struct_extract",
  "subtype",
  "st_area",
  "area",
  "geometry",
  "desc"
))
