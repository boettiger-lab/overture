library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(dplyr)
library(duckdbfs)
library(colourpicker)
#library(overture)

devtools::load_all()
#  options("overture-bucket" = "public-overturemaps")

## AWS S3
duckdbfs::duckdb_secrets(key = "", secret = "",
                         endpoint = "s3.amazonaws.com",
                         bucket = "overturemaps-us-west-2")

options("overture-bucket" = "overturemaps-us-west-2",
        "overture-release" = "2025-07-23.0")

ui <- page_sidebar(
  title = "mapgl with Shiny",
  sidebar = sidebar(
    textInput("feature", "Location", "United States"),
    colourInput("color", "Select a color",
                value = "blue"),
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

observeEvent(input$feature, {

    print(input$feature)
    new_gdf <- get_division(input$feature)
    bounds <- as.vector(sf::st_bbox(new_gdf))

    maplibre_proxy("map") |>
      set_source(layer_id = "gdf_layer",
                 source = new_gdf) |>
                 fit_bounds(bounds, animate = TRUE)
})  %>% debounce(millis = 600)

observeEvent(input$color, {
    maplibre_proxy("map") |>
        set_paint_property("gdf_layer", "fill-color", input$color)
})

}

shinyApp(ui, server)