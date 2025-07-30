devtools::load_all()
library(dplyr)

duckdbfs::duckdb_secrets()
options("overture-bucket" = "public-overturemaps")

primary_name <- "United States"
gdf <- overture::overture("divisions", "division_area") |> 
  filter(subtype == "country") |>
  filter(struct_extract(names,"primary") == {primary_name}) |> 
  safe_gdf()



gdf <- get_subdivision(primary_name = "United States")



path <- "public-data/cache/overture.geojson"

url <- gdf |> to_s3(path)
map(url)



