
devtools::load_all()
options("overture-bucket" = "public-overturemaps")

d <- division()



bench::bench_time({
  get_subdivision("Montevideo", "UY")
})

bench::bench_time({
  gdf <- get_subdivision(primary_name = "San Fransisco")
  map(gdf)
})  

map(gdf)

children <- d_id |> inner_join(has_parent) 

gdf <- d |>  filter(country == "UY") |> inner_join(children) |> safe_gdf()


gdf |>  map()



View(gdf)


d |> filter(country == "US") |> count(subtype) |> collect()

d |> filter(country == "US", subtype == "region")

d |> filter(region == "US-CA", subtype == "county") 

d |> filter(region == "US-CA") |> 
  mutate(primary = struct_extract(names,"primary")) |>
  filter(grepl("San Diego", primary)) |> 
  select(id, division_id, primary)


d |> filter(region == "US-CA") |> (subtype) |> collect()
  



ca_locality <- d |> filter(region == "US-CA", subtype == "locality") 


## Who has San Diego County's division_id as it's parent division?
gdf <- d_id |> 
  filter(parent_division_id == {division_id}) |>
  select(division_id = id) |>
  inner_join(ca_locality) 

g <- safe_gdf(gdf)

map(gdf, fill_outline_color = "black")


mapgl::maplibre(bounds = g) |>
  mapgl::add_fill_layer(
    tooltip = "primary",
    source = g,
    fill_color = "blue",
    fill_opacity = 0.2,
#    fill_outline_color = "black",

  )

