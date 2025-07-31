devtools::load_all()
library(dplyr)

#duckdbfs::duckdb_secrets()
#options("overture-bucket" = "public-overturemaps")

## AWS S3
duckdbfs::duckdb_secrets(key = "", secret = "",
                         endpoint = "s3.amazonaws.com",
                         bucket = "overturemaps-us-west-2")
options("overture-bucket" = "overturemaps-us-west-2",
        "overture-release" = "2025-07-23.0")

ca <- get_division("California", type = "region")
us <- get_division("United States")

# plot with sf
library(sf)
plot(ca[1])

# maplibre plotting helper
map(ca)


# a county
yolo <- get_division("Yolo County",  as_sf = FALSE)
map(yolo)


# using urls
path <- "public-data/cache/overture.geojson"
url <- yolo |> to_s3(path)
map(url)

## Using filters instead of joins...

# get all children of a given id


options("overture-bucket" = "public-overturemaps",
        "overture-release" = "2025-07-23.0")


duckdbfs::duckdb_secrets()

bench::bench_time({
usa <- get_division_("United States")
parent_id <- us |> pull(division_id) # must use division id, not id
 
children <- 
  overture("divisions", "division") |> 
  filter(parent_division_id == !!parent_id) |>
  pull(id)

polys <- 
  overture("divisions", "division_area") |> 
  filter(division_id %in% children) |>
  safe_gdf()

})

bench::bench_time({
children <- 
  overture("divisions", "division") |> 
  filter(parent_division_id == !!parent_id) |>
  select(division_id = id) |>
  inner_join(overture("divisions", "division_area")) |> 
  safe_gdf()
})
bench::bench_time(gdf <- safe_gdf(polys))

map(polys)

# raw direct access
# Configure to s3 to ignore any exsiting env vars.
duckdbfs::duckdb_secrets(key = "", secret = "", endpoint = "s3.amazonaws.com", bucket = "overturemaps-us-west-2")
duckdbfs::open_dataset("s3://overturemaps-us-west-2/release/2025-07-23.0/theme=base/type=land_cover/*")

