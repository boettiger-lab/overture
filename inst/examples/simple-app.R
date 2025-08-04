library(shiny)
library(bslib)
library(mapgl)
library(sf)
library(dplyr)
library(duckdbfs)
library(colourpicker)
library(overture)

counties <- "https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_county.pmtiles"
tract <- "https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.pmtiles"

## Make anonymous AWS S3 default endpoint for "overturemaps-us-west-2"
duckdbfs::duckdb_secrets(
  key = "",
  secret = "",
  endpoint = "s3.amazonaws.com",
  bucket = "overturemaps-us-west-2"
)


ui <- page_sidebar(
  title = "Interactive feature selection",
  # Add custom CSS for smaller font size

  sidebar = sidebar(
    card(
      card_header("Layers"),
      input_switch("show_states", "States", value = FALSE),
      input_switch("show_counties", "Counties", value = FALSE),
    ),

    card(
      textInput("feature", "Find:", NULL),
      #actionButton("overture_layer", "go"),
    ),

    hr(),
    p("Actions"),
    actionButton("get_features", "Get drawing"),
    actionButton("visible_features", "Get current features"),
    actionButton("current_bbox", "Get bbox"),

    hr(),
    br(),

    colourInput("color", "Select a color", value = "blue"),
    sliderInput(
      "max_zoom",
      "Max Zoom Level",
      min = 1,
      max = 15,
      value = 6,
      step = 1
    ),
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  ),
  includeMarkdown(
    "A demonstration of different mechanisms to interactively select features in mapgl + shiny.

  - Toggle off and on possible selection layers (States, counties)
  - Use a PMTiles layer to render many polygons quickly (counties)
  - Draw a polygon on a map
  - extract points from the built-in geocode
  - query Overture Maps for a polygon
  - Select a feature with mouse from PMTiles or sf layer
  - Grab visible features (including filters) ('local' sources only?)
  "
  ),
)


server <- function(input, output, session) {
  output$map <- renderMaplibre({
    m <- mapgl::maplibre(zoom = 1, maxZoom = input$max_zoom)

    # PMTiles sources MUST be added at the start
    # Good to add all sources at start, then toggle with layer controls
    m <- m |>
      add_pmtiles_source(id = "county_source", url = counties) |>
      add_source("states_source", spData::us_states)

    m <- m |>
      add_geocoder_control() |>
      add_draw_control() |>
      add_fullscreen_control() |>
      add_globe_control()

    # Add any layer you want to be on by default

    m
  })

  # Guess layer name of PMTiles file so we don't have to manually enter
  suppressWarnings({
    layer_name <- sf::st_layers(paste0("/vsicurl/", counties))$name[1]
  })

  observeEvent(input$show_counties, {
    if (input$show_counties) {
      maplibre_proxy("map") |>
        add_fill_layer(
          id = "county_layer",
          source = "county_source",
          source_layer = layer_name,
          fill_opacity = 0.05,
          tooltip = concat(
            "Name: ",
            get_column("COUNTY"),
            "<br>STATE: ",
            get_column("ST_ABBR"),
            "<br>FIPS: ",
            get_column("FIPS")
          )
        )
    }
    if (!input$show_counties) {
      maplibre_proxy("map") |> clear_layer("county_layer")
    }
  })

  # Ex: Toggle counties layer
  observeEvent(input$show_states, {
    if (input$show_states) {
      gdf <- spData::us_states

      maplibre_proxy("map") |>
        add_fill_layer(
          id = "states_layer",
          source = "states_source",
          fill_opacity = 0.3,
          fill_color = "purple"
        ) |>
        fit_bounds(gdf, animate = TRUE)
    } else {
      maplibre_proxy("map") |> clear_layer("states_layer")
    }
  })

  # Ex: Select the feature the user clicked on and zoom into it
  # This reacts to drawing features too
  observeEvent(input$map_feature_click, {
    my_layers <- c("states_layer", "county_layer")
    x <- input$map_feature_click
    if (x$layer %in% my_layers) {
      print(x)
    }
    # use x$layer and x$properties$FIPS ( ID column) to extract geom and plot
  })

  # Ex: Get a feature by name from Overture
  # really we might want to filter to country, specify type, and fuzzy-match on names
  observeEvent(input$feature, {
    print(paste("Searching Overture for area:", input$feature))

    #
    new_gdf <- overture::get_division(input$feature)

    print(new_gdf)
    if (nrow(new_gdf) < 1) {
      print(paste("No exact match for primary name:", input$feature))
      maplibre_proxy("map")
    } else {
      bounds <- as.vector(sf::st_bbox(new_gdf))

      # remove any existing overture layer first
      maplibre_proxy("map") |> clear_layer("overture")

      maplibre_proxy("map") |>
        add_fill_layer(
          id = "overture",
          source = new_gdf,
          fill_opacity = 0.3,
          fill_color = "pink"
        ) |>
        fit_bounds(bounds, animate = TRUE)
    }
  }) |>
    debounce(millis = 3000)
  # more time to type?

  # Ex Show a feature user has drawn on the map
  observeEvent(input$get_features, {
    print("Extracting drawn features")

    drawn_features <- get_drawn_features(maplibre_proxy("map"))
    print(drawn_features)
  })

  observeEvent(input$current_bbox, {
    bbox <- sf::st_bbox(unlist(input$map_bbox), crs = 4326)
    print("current bbox:")
    print(bbox)
  })

  # Ex: Get POINT data from geocoder (OSM)
  # could then react by operating on hex or some containing polygon
  observeEvent(input$map_geocoder$result, {
    temp <- tempfile(fileext = ".geojson")
    output <- list(
      type = "FeatureCollection",
      features = input$map_geocoder$result$features
    )
    jsonlite::write_json(
      output,
      temp,
      auto_unbox = TRUE
    )
    geo <- sf::st_read(temp)
    print(geo)
  })

  # Get current features from a specified layer
  # GDF layer only (not proxy URL / PMTiles layers)
  observeEvent(input$visible_features, {
    print("Extracting current features...")
    proxy <- maplibre_proxy("map")
    # layer_id be multiple layers or all layers
    query_rendered_features(proxy)
    features <- get_queried_features(proxy)
    print(head(features))
  })

  # Ex: Update the fill color
  observeEvent(input$color, {
    maplibre_proxy("map") |>
      set_paint_property("counties_layer", "fill-color", input$color)
  })
}

shinyApp(ui, server)
