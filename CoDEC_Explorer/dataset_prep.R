library(codec)
library(dplyr)

d_drive <- codec_data("hamilton_drivetime") |> 
  select(-year) 

d_land <- codec_data("hamilton_landcover") |> 
  select(-year) 

d_traffic <- codec_data("hamilton_traffic") |> 
  select(-year) 

d_acs <- codec_data("hh_acs_measures") |> 
  filter(year == max(year)) |> 
  select(-contains("moe"), -year)
  
d_indices <- codec_data("tract_indices") |> 
  select(-year) 

d_property <- codec_data("hamilton_property_code_enforcement") |> 
  select(-year) 


d_all <- left_join(d_drive, d_land, by = "census_tract_id_2010") |> 
  left_join(d_traffic, by = "census_tract_id_2010") |> 
  left_join(d_acs, by = "census_tract_id_2010") |> 
  left_join(d_indices, by = "census_tract_id_2010") |> 
  left_join(d_property, by = c("census_tract_id_2010" = "census_tract_id_2020")) 

d_all_log <- d_all |> 
  select(where(is.logical)) |> 
  pivot_longer(cols = everything(), names_to = 'name', values_to = 'value') |> 
  distinct(name)

d_all <- d_all |> 
  select(!where(is.logical)) 

d_all <- d_all |> 
  left_join(tigris::tracts(state = 'OH', county = "Hamilton", year = 2010), 
                     by = c('census_tract_id_2010' = 'GEOID10')) |> 
    # left_join(cincy::tract_tigris_2010, by = 'census_tract_id_2010') |> 
     sf::st_as_sf(crs = '4326')

var_meta <- glimpse_schema(d_all) |> 
  relocate(title, .before = name) |> 
  rowwise() |> 
  mutate(title = coalesce(title, name)) |> 
  ungroup()

var_meta$description <- var_meta$description |> 
  replace_na("No description available")

d_indices_names <- d_indices |> 
  pivot_longer(cols = -census_tract_id_2010, names_to = "name", values_to = "value") |> 
  mutate(core = "tract_indices") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_drive_names <- d_drive |> 
  pivot_longer(cols = -census_tract_id_2010, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_drivetime") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_land_names <- d_land |> 
  pivot_longer(cols = -census_tract_id_2010, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_landcover") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_traffic_names <- d_traffic |> 
  pivot_longer(cols = -census_tract_id_2010, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_traffic") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_acs_names <- d_acs |> 
  pivot_longer(cols = -census_tract_id_2010, names_to = "name", values_to = "value") |> 
  mutate(core = "hh_acs_measures") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_property_names <- d_property |> 
  pivot_longer(cols = -census_tract_id_2020, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_property_code_enforcement") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_names <- rbind(d_drive_names, d_land_names, d_traffic_names, d_acs_names, d_indices_names, d_property_names) |> 
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
  glimpse_tdr(d_indices)$attributes,
  glimpse_tdr(d_property)$attributes
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



  