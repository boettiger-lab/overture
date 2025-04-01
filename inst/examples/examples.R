#library(overture)

library(dplyr)
library(duckdbfs)
library(sf)
devtools::load_all()

d <- division()

d |> filter(subtype == "country") |> select(id, country, names) |> collect()

d |> filter(country == "US") |> count(subtype) |> collect()

d |> filter(country == "US", subtype == "region")

gdf <- d |> filter(region == "US-CA", subtype == "county") |> select("id", "geometry") |> to_sf(crs="epsg:4326") 

g <- gdf
library(mapgl)
maplibre(bounds = g) |>
  add_fill_layer(
    id = "gdf_data",
    source = g,
    fill_color = "blue",
    fill_opacity = 0.1
  )
