library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

oh_bos_507 <- st_read("Ohio/OH_507/OH_507.shp")
ohio_counties <- st_read("Ohio/REFER_COUNTY.shp")

tm_shape(oh_bos_507) + 
  tm_fill() + 
  
  tm_shape(ohio_counties) + 
  tm_polygons()
  
