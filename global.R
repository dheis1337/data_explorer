library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)
library(shinyWidgets)
library(rlang)
library(scales)
library(mapdeck)
library(leaflet.mapboxgl)
library(shinythemes)

# read in denv_explore file
# denv_explor <- fread("~/MyStuff/DataScience/data_explorer/denv_explore.csv",
#                      stringsAsFactors = FALSE)

denv_explor <- fread("C:/my_stuff/data_explorer/mls_props.csv", stringsAsFactors = FALSE,
                     na.strings = "") %>%
  separate(col = "proj_coords", into = c("x_proj", "y_proj"), sep = "\\|") %>%
  mutate(x_proj = as.numeric(x_proj), y_proj = as.numeric(y_proj)) %>%
  st_as_sf(coords = c("x_proj", "y_proj"), dim = "XY", na.fail = FALSE) %>%
  mutate(architecture_style = factor(architecture_style),
         construction_material = factor(construction_material),
         property_type = factor(property_type), 
         property_condition = factor(property_condition)) %>%
  st_set_crs(26913)


 
zips <- c(80001:80299, 80601, 80602, 80614, 80640, 80401, 80402, 80403, 80419)


non_zips <- c(80101, 80117, 80105, 80132, 80133, 80102, 80103, 80026, 80025, 80137, 
              80136, 80131, 80026, 80019, 80106, 80138, 80116, 80135, 80109)

zips <- zips[!(zips %in% non_zips)]


zip <- st_read("C:/my_stuff/data_explorer/Colorado_ZIP_Code_Tabulation_Areas_ZCTA.shp",
               stringsAsFactors = FALSE) %>%
  select("ZCTA5CE10", "geometry") %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
  filter(ZCTA5CE10 %in% zips) %>%
  arrange(ZCTA5CE10)
  

zip <- rmapshaper::ms_simplify(input = as(zip, 'Spatial')) %>% st_as_sf()    
  
cities <- unique(denv_explor$prop_addr_city)

non_cities <- c("Kittredge", "Idaho Springs", "Black Hawk", "Ward", "Dacono", 
                "Lafayette", "Hudson", "Fort Lupton", "Erie", "Lochbuie", 
                "Bennett", "Byers", "Strasburg", "Loveland", "Larkspur", 
                "Woodland Park", "Palmer Lake", "Eibert", "Deer Trail", 
                "Brush", "Wiggins", "Fort Morgan", "Evans", "Loveland",
                "Dillon", "Crawford", "Windsor", "Frederick", "Evergreen",
                "Keenesburg", "Elizabeth", "Boulder", "Superior", "Watkins", 
                "Louviers", "Sedalia", "Franktown", "Foxfield", "Dinosaur", 
                "Idledale", "Indian Hills", "Morrison")

cities <- cities[!(cities %in% non_cities)]



city <- st_read("C:/my_stuff/data_explorer/Colorado_City_Boundaries.shp",
        stringsAsFactors = FALSE) %>%
  select("NAME10", "geometry") %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
  filter(NAME10 %in% cities) %>%
  arrange(NAME10)


city <- rmapshaper::ms_simplify(input = as(city, 'Spatial')) %>% st_as_sf()    



