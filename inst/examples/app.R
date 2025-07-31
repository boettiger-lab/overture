## recursively view parent

library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(dplyr)
library(duckdbfs)
library(rlang)
library(overture)

devtools::load_all()
options("overture-bucket" = "public-overturemaps",
        "overture-release" = "2025-07-23.0")
duckdbfs::duckdb_secrets()

ui <- page_sidebar(
  title = "mapgl with Shiny",
  sidebar = sidebar(
    textInput("feature", NULL, "California"),
    textOutput("clicked_feature"),
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {

  gdf <- get_division("United States")
  bounds <- as.vector(sf::st_bbox(gdf))

  output$map <- renderMaplibre({
    map(gdf)
  })

  # React to text input location
  observeEvent(input$feature, {
    new_gdf <- get_division(input$feature)
    bounds <- as.vector(sf::st_bbox(new_gdf))

    maplibre_proxy("map") |>
      set_source(layer_id = "gdf_layer",
                source = new_gdf) |>
                fit_bounds(bounds, animate = TRUE)
  }) |> 
  debounce(millis = 600) # give time to type

  # React to clicked feature, showing children of the feature
  observeEvent(input$map_feature_click, {
    parent_id <- input$map_feature_click$properties$division_id
    print(parent_id)

    division_ids <- overture("divisions", "division")
    division_areas <- overture("divisions", "division_area")

    # Fully local
    division_ids <- duckdbfs::open_dataset("/home/jovyan/data/overturemaps/release/2025-07-23.0/theme=divisions/type=division")
    division_areas <- duckdbfs::open_dataset("/home/jovyan/data/overturemaps/release/2025-07-23.0/theme=divisions/type=division_area")

    children <- 
      division_ids |> 
      filter(parent_division_id == !!parent_id) |>
      pull(id)

    gdf <- 
      division_areas |> 
      filter(division_id %in% children) |>
      filter(is_land)

#    gdf <- division_ids |> 
#      filter(parent_division_id == !!parent_id) |>
#      select(division_id = id) |>
#      inner_join(division_areas, by ="division_id") |> 
#      filter(is_land)

    path <- glue::glue("public-data/cache/{parent_id}.geojson")
    ## FIXME currently duckdbfs::to_geojson includes only id, not all attributes
    url <- gdf |> to_s3(path, id_col = "division_id")
    print(url)

    maplibre_proxy("map") |>
      set_source("gdf_layer", url)
  })
}

shinyApp(ui, server)