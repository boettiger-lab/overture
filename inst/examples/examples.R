devtools::load_all()
library(dplyr)

duckdbfs::duckdb_secrets()
options("overture-bucket" = "public-overturemaps")


ca <- get_division("California", type = "region")
uk <- get_division("United Kingdom")

# plot with sf
library(sf)
plot(ca[1])

# maplibre plotting helper
map(ca)

# a county
yolo <- get_division("Yolo County")
map(yolo)

gdf <- get_subdivision(primary_name = "United States")

# using urls
path <- "public-data/cache/overture.geojson"
url <- gdf |> to_s3(path)
map(url)


# raw direct access
# Configure to s3 to ignore any exsiting env vars.
duckdbfs::duckdb_secrets(key = "", secret = "", endpoint = "s3.amazonaws.com", bucket = "overturemaps-us-west-2")
duckdbfs::open_dataset("s3://overturemaps-us-west-2/release/2025-07-23.0/theme=base/type=land_cover/*")

