


subtype = c(
'country',
'dependency',
'region',
'county',
'localadmin',
'locality',
'macrohood',
'neighborhood',
'microhood')

division <- function(geom_type = c("polygon", "point", "line")) {
  geom_type <- match.arg(geom_type)
  division_geom_type <- switch(geom_type, 
                               polygon = "division_area",
                               point = "division",
                               line = "division_boundary",
                               "division_area")
  
  overture("divisions", division_geom_type)
}

# US "States" are Regions, regions are encoded as US-CA
# counties do not have a filter