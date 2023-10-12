library(codec)
library(dplyr)


d_drive <- codec_data("hamilton_drivetime", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  select(-year) 

d_land <- codec_data("hamilton_landcover", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  select(-year) 

d_traffic <- codec_data("hamilton_traffic", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  select(-year)

d_acs <- codec_data("hh_acs_measures", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  filter(year == max(year)) |> 
  select(-contains("moe"), -year) 

# d_property <- codec_data("hamilton_property_code_enforcement", geography = cincy::tract_tigris_2010, geometry = FALSE) |>
#   select(-year)
  
d_indices <- codec_data("tract_indices", geography = cincy::tract_tigris_2010, geometry = TRUE) |> 
  sf::st_as_sf() |>
  sf::st_transform(crs = 5072) |> 
  select(-year) 


d_all <- left_join(d_drive, d_land, by = "census_tract_id_2010") |> 
  left_join(d_traffic, by = "census_tract_id_2010") |> 
  left_join(d_acs, by = "census_tract_id_2010") |> 
  #left_join(d_property, by = "census_tract_id_2010") |> 
  left_join(d_indices, by = "census_tract_id_2010") |> 
  rename('geo_index' = census_tract_id_2010)

d_all_log <- d_all |> 
  select(where(is.logical)) |> 
  pivot_longer(cols = everything(), names_to = 'name', values_to = 'value') |> 
  distinct(name)

d_all <- d_all |> 
  select(!where(is.logical)) 

d_all <- d_all |> 
  sf::st_as_sf() |> 
  sf::st_transform(4326)

var_meta <- glimpse_schema(d_all) |> 
  relocate(title, .before = name) |> 
  rowwise() |> 
  mutate(title = coalesce(title, name)) |> 
  ungroup()

var_meta$description <- var_meta$description |> 
  replace_na("No description available")


get_names <- function(d, core_name) {
  d <- d |> 
    sf::st_drop_geometry() |>
    pivot_longer(cols = -census_tract_id_2010, names_to = "name", values_to = "value") |> 
    mutate(core = core_name) |> 
    select(name, core) |> 
    left_join(var_meta, by = "name")
  
  d
}

d_indices_names <- d_indices |>
  get_names('tract_indices')

d_drive_names <- d_drive |> 
  get_names('hamilton_drivetime')

d_land_names <- d_land |> 
  get_names('hamilton_landcover')

d_traffic_names <- d_traffic |> 
  get_names('hamilton_traffic')

d_acs_names <- d_acs |> 
  get_names('hh_acs_measures')

 # d_property_names <- d_property |> 
 #   get_names('hamilton_property_code_enforcement')

d_names <- rbind(d_drive_names, d_land_names, d_traffic_names, d_acs_names, d_indices_names) |>#, d_property_names) |> 
  distinct()

d_names <- d_names |> 
  relocate(title, .before = name) |> 
  rowwise() |> 
  mutate(title = coalesce(title, name)) |> 
  ungroup()

d_names <- d_names |> 
  filter(!name %in% d_all_log$name)


core_meta <- rbind(
  glimpse_tdr(d_drive)$attributes,
  glimpse_tdr(d_land)$attributes,
  glimpse_tdr(d_traffic)$attributes,
  glimpse_tdr(d_acs)$attributes,
  glimpse_tdr(d_indices)$attributes#,
  #glimpse_tdr(d_property)$attributes
) 

core_names <- tibble("title" = filter(core_meta, name == 'title')$value, "name" = filter(core_meta, name == 'name')$value) 



d_names_dupes <- d_names |> 
  left_join(core_names, by = c('core' = 'name')) |> 
  rename(title = "title.x", core_title = "title.y") |> 
  filter(duplicated(title)) |> 
  mutate(title = paste0(title, " - ", core_title)) 

d_names <-  d_names |> 
  left_join(core_names, by = c('core' = 'name')) |> 
  rename(title = "title.x", core_title = "title.y") |> 
  filter(!duplicated(title)) |> 
  rbind(d_names_dupes)

  