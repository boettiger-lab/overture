devtools::load_all()
library(dplyr)
options("overture-bucket" = "public-overturemaps")
gdf <- get_subdivision(primary_name = "United States")

path <- "public-data/cache/overture.geojson"

gdf |> sf::st_write(paste0("/vsis3/", path))


url <- paste0("https://", Sys.getenv("AWS_S3_ENDPOINT"), "/", path)

mapgl::maplibre(bounds = gdf) |>
  mapgl::add_source("s3_data", url) |>
  mapgl::add_fill_layer(
    id = "layer",
    tooltip = "primary",
    source = "s3_data",
    fill_color = "blue",
    fill_opacity = 0.2,
#    fill_outline_color = "black",

  )

