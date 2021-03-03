library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

oh_bos_507 <- st_read("Ohio/OH_507/OH_507.shp")
oh_mah_504 <- st_read("Ohio/OH_504/OH_504.shp")
ohio_counties <- st_read("Ohio/REFER_COUNTY.shp")

tm_shape(ohio_counties) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(oh_bos_507) +
  tm_fill(col = "#00264d",
          alpha = .8) +
  tm_layout(
    title = "Ohio Balance of State CoC",
    title.position = c("center", "top"),
    inner.margins = .08,
    frame = FALSE
  )

tm_shape(ohio_counties) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(oh_mah_504,
           alpha = .8) +
  tm_fill(col = "#00264d") +
  tm_layout(
    title = "Mahoning County CoC",
    title.position = c("center", "top"),
    inner.margins = .08,
    frame = FALSE
  )
