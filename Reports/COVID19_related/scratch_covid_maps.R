library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(here)

oh_bos_507 <- st_read(here("Ohio/OH_507/OH_507.shp"))
oh_mah_504 <- st_read(here("Ohio/OH_504/OH_504.shp"))
ohio_counties <- st_read(here("Ohio/Counties/REFER_COUNTY.shp"))

tm_shape(ohio_counties) +
  tm_fill(col = "gray") +
  tm_borders() +
  tm_shape(oh_bos_507) +
  tm_borders(col = "black", lwd = 2.5, lty = "longdash") +
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
  tm_shape(oh_mah_504) +
  tm_borders(col = "black", lwd = 2.5, lty = "longdash") +
  tm_fill(col = "#00264d",
           alpha = .8) +
  tm_layout(
    title = "Mahoning County CoC",
    title.position = c("center", "top"),
    inner.margins = .08,
    frame = FALSE
  )
