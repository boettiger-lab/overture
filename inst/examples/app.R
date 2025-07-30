library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(dplyr)
library(duckdbfs)
#library(overture)

devtools::load_all()
options("overture-bucket" = "public-overturemaps")


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

  output$map <- renderMaplibre({
    hostpath = "public-data/cache/overture.geojson"
    url <- paste0("https://", Sys.getenv("AWS_S3_ENDPOINT"), "/", hostpath)

    get_subdivision(primary_name = "United States") |>
      duckdbfs::to_geojson(paste0("s3://", hostpath), id_col = "primary")

    print(url)

    maplibre(style = carto_style("positron")) |>
      add_fill_layer(id = "gdf_layer",
                     source = url,
                     fill_color = "blue",
                     fill_opacity = 0.5)
  })

  output$clicked_feature <- renderText({
    req(input$map_feature_click)
    input$map_feature_click
  })

  observeEvent(input$feature, {
    hostpath = "public-data/cache/overture1.geojson"
    url <- paste0("https://", Sys.getenv("AWS_S3_ENDPOINT"), "/", hostpath)
    get_subdivision(input$feature) |>  
      to_geojson(paste0("s3://", hostpath), id_col = "primary")

    maplibre_proxy("map") |>
      set_source("gdf_layer", url)
  })


  observeEvent(input$map_feature_click, {
    name = input$map_feature_click$properties$primary

    hostpath = "public-data/cache/overture2.geojson"
    url <- paste0("https://", Sys.getenv("AWS_S3_ENDPOINT"), "/", hostpath)
    gdf <- get_subdivision(name) |>
      to_geojson(paste0("s3://", hostpath), id_col = "primary")

    maplibre_proxy("map") |>
      set_source("gdf_layer", url)
  })
}

shinyApp(ui, server)