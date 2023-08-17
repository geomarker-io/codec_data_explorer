library(codec)
library(dplyr)

# if (exists(input$sel_geo) == F) {
#    selected_geo <- 'tract'
# } else {
#   selected_geo <- input$sel_geo
# }

#geo_option <- tibble("geo" = c("tract", "zcta", "neighborhood"),
 #                    "cincy_name" = list(cincy::tract_tigris_2010, cincy::zcta_tigris_2010, cincy::neigh_cchmc_2010))

#collect_data <- function(selected_geo) {
# 
# geo_input <- geo_option |> 
#   filter(geo == selected_geo) |> 
#   pull(cincy_name) |> 
#   pluck(1)

d_drive <- codec_data("hamilton_drivetime", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  select(-year) %>%
  rename('geo_index' = colnames(.)[1])

d_land <- codec_data("hamilton_landcover", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  select(-year) %>%
  rename('geo_index' = colnames(.)[1])

d_traffic <- codec_data("hamilton_traffic", geography = cincy::tract_tigris_2010, geometry = FALSE) |> 
  select(-year) %>%
  rename('geo_index' = colnames(.)[1])

d_acs <- codec_data("hh_acs_measures", cincy::tract_tigris_2010, geometry = FALSE) |> 
  filter(year == max(year)) |> 
  select(-contains("moe"), -year) %>%
  rename('geo_index' = colnames(.)[1])
  
d_indices <- codec_data("tract_indices", geography = cincy::tract_tigris_2010, geometry = TRUE) |> 
  sf::st_as_sf() |>
  sf::st_transform(crs = 5072) |> 
  select(-year) %>%
  rename('geo_index' = colnames(.)[1])
# 
# d_property <- codec_data("hamilton_property_code_enforcement", geography = geo_input, geometry = FALSE) |> 
#   select(-year) %>%
#   rename('geo_index' = colnames(.)[1])
#property code only available in 2020, not sure how to reconcile together


d_all <- left_join(d_drive, d_land, by = "geo_index") |> 
  left_join(d_traffic, by = "geo_index") |> 
  left_join(d_acs, by = "geo_index") |> 
  left_join(d_indices, by = "geo_index") #|> 
 # left_join(d_property, by = "geo_index") 

d_all_log <- d_all |> 
  select(where(is.logical)) |> 
  pivot_longer(cols = everything(), names_to = 'name', values_to = 'value') |> 
  distinct(name)

d_all <- d_all |> 
  select(!where(is.logical)) 

d_all <- d_all |> 
   sf::st_as_sf()



 # left_join(tigris::tracts(state = 'OH', county = "Hamilton", year = 2010), 
            #         by = c('census_tract_id_2010' = 'GEOID10')) |> 
    # left_join(cincy::tract_tigris_2010, by = 'census_tract_id_2010') |> 

var_meta <- glimpse_schema(d_all) |> 
  relocate(title, .before = name) |> 
  rowwise() |> 
  mutate(title = coalesce(title, name)) |> 
  ungroup()

var_meta$description <- var_meta$description |> 
  replace_na("No description available")




d_indices_names <- d_indices |> 
  sf::st_drop_geometry() |>
  pivot_longer(cols = -geo_index, names_to = "name", values_to = "value") |> 
  mutate(core = "tract_indices") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_drive_names <- d_drive |> 
  pivot_longer(cols = -geo_index, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_drivetime") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_land_names <- d_land |> 
  pivot_longer(cols = geo_index, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_landcover") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_traffic_names <- d_traffic |> 
  sf::st_drop_geometry() |>
  pivot_longer(cols = -geo_index, names_to = "name", values_to = "value") |> 
  mutate(core = "hamilton_traffic") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

d_acs_names <- d_acs |> 
  sf::st_drop_geometry() |>
  pivot_longer(cols = -geo_index, names_to = "name", values_to = "value") |> 
  mutate(core = "hh_acs_measures") |> 
  select(name, core) |> 
  left_join(var_meta, by = "name")

# d_property_names <- d_property |> 
#   pivot_longer(cols = -census_tract_id_2020, names_to = "name", values_to = "value") |> 
#   mutate(core = "hamilton_property_code_enforcement") |> 
#   select(name, core) |> 
#   left_join(var_meta, by = "name")

d_names <- rbind(d_drive_names, d_land_names, d_traffic_names, d_acs_names, d_indices_names) |> #, d_property_names
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



#out <- list(d_all, var_meta, core_names, d_names)

#return(out)

#}


  