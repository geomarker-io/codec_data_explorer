library(codec)
# requiring a specific version of codec for now:
# renv::install("geomarker-io/codec@8a84bf4")

library(cincy)
## renv::install("geomarker-io/cincy@v1.0.0")

library(dplyr)
library(purrr)
library(fs)
library(sf)

# read data from installed codec package
codec_data <-
  path_package("codec") |>
  path("codec_data") |>
  dir_ls(glob = "*tabular-data-resource.yaml", recurse = TRUE) |>
  map(read_tdr_csv, .progress = "reading codec data")

# get year column for each resource

census_tract_id_names <- paste0("census_tract_id", c("_2000", "_2010", "_2020"))

codec_data_tract_years <-
  codec_data |>
  map_chr(\(.) census_tract_id_names[which(census_tract_id_names %in% names(.))
  ]) |>
  strsplit("_", fixed = TRUE) |>
  map_chr(4)

codec_data_tracts <-
  glue::glue("tract_tigris_{codec_data_tract_years}") |>
  rlang::syms() |>
  map(eval, envir = .GlobalEnv)

codec_data_cincy <-
  pmap(list("x" = codec_data,
            "y" = codec_data_tracts,
            "by" = paste0("census_tract_id_", codec_data_tract_years)),
       left_join) |>
  map(sf::st_as_sf)

codec_data_2020 <-
  codec_data_cincy |>
  map_if(! codec_data_tract_years == "2020",
         \(.) cincy::interpolate(from = ., to = cincy::tract_tigris_2020, weights = "pop"),
         .progress = "interpolating to 2020 census tracts")

codec_data_2020_nosf <- codec_data_2020 |> 
  st_drop_geometry()

write_rds(codec_data_2020_nosf, '2020_all_codec_nosf.rds')
