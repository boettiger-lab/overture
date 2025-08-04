f <- glue::glue
endpoint <- Sys.getenv("AWS_PUBLIC_ENDPOINT", "minio.carlboettiger.info")

county_data <- f(
    "https://{endpoint}/public-social-vulnerability/2022/",
    "SVI2022_US_county.parquet"
)

#oregon_gdf <- duckdbfs::open_dataset(county_data, recursive = FALSE) |>
#    dplyr::filter(ST_ABBR == "CA") |>
#    rename(geom = Shape) |>
#    duckdbfs::to_sf()

county_tiles <- f(
    "https://{endpoint}/public-social-vulnerability/2022/",
    "SVI2022_US_county.pmtiles"
)
tract_tiles <- f(
    "https://{endpoint}/public-social-vulnerability/222/",
    "SVI2022_US_tract.pmtiles"
)

# Go nuts with overture buildings, places, addresses?

## Really we should have 
## select-box for Overture (sub)type.  Should support all types instead?
  tags$div(
        style = "font-size: 0.7em;",
        checkboxGroupInput(
          "location_type",
          NULL,
          c(
            "Country" = "country",
            "Region" = "region",
            "County" = "county",
            "Locality" = "locality"
          ),
          selected = c("country", "region")
        )
      ),