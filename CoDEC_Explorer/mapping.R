library(dplyr)
library(codec)
library(sf)

d <- codec_data("tract_indices") |> 
 left_join(tigris::tracts(state = 'OH', county = "Hamilton", year = 2010), 
           by = c('census_tract_id_2010' = 'GEOID10')) |> #left_join(cincy::tract_tigris_2010) |> 
  sf::st_as_sf()

#there's something goofy with the projection of the tracts coming from the cincy package, unplottable

bins_x <- pull(d, ice)
bins_y <- pull(d, dep_index)

bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")

bins_x <- bins_x$brks
bins_y <- bins_y$brks

# cut into groups defined above
out <- d |> 
  mutate(bi_x = cut(ice, breaks = bins_x, include.lowest = TRUE))
out <- out |> 
  mutate(bi_y = cut(dep_index, breaks = bins_y, include.lowest = TRUE))
out <- out|> 
  mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))

codec_colors()
"#396175"    "#58829C"    "#8CB4C3"    "#CBD6D5"    "#F6EDDE"    "#EACEC5"    "#E49865"    "#C28273"
codec_colors <- c(codec_colors(), "#FFFFFF")
names(codec_colors) <- c("1-1", "1-2", "1-3", "2-1", "2-3", "3-1", "3-2", "3-3", "2-2")
#bivariate colors generated from codec scale here: https://observablehq.com/@benjaminadk/bivariate-choropleth-color-generator
["#eddcc1", "#d4aa92", "#bb7964", "#909992", "#81766f", "#71544c", "#375a66", "#31464d", "#2b3135"]

library(tmap)
library(leaflet)
tmap_mode("view")

codec_pal <- colorFactor(codec_colors, out$bi_class)

out <- sf::st_as_sf(out, sf_column_name = "geometry" ) 
  

st_crs(out)
mapview::mapview(d, zcol = "adi")

map <- 
  tm_basemap("CartoDB.Positron") +
  tm_shape(out, unit = 'miles') +
  tm_polygons(col ="bi_class", alpha = 0.4, palette = codec_colors, legend.show = FALSE)

map |> 
  tmap_leaflet()

