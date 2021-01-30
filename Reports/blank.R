# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(ggplot2)
library(dplyr)

# Get the world polygon and extract UK
library(maps)
UK <- map_data("world") %>% filter(region=="UK")

# load(here("images/COHHIOHMIS.RData"))
# virids package for the color palette
library(viridis)
data <- world.cities %>% filter(country.etc=="UK")
# Left: use size and color
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() 

# Center: reorder your dataset first! Big cities appear later = on top
data %>%
  arrange(pop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

# Right: just use arrange(desc(pop)) instead
data %>%
  arrange(desc(pop)) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

# trying something else ---------------------------------------------------
# https://people.ohio.edu/ruhil/Rbook/maps-in-r.html

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(ggthemes)

usa = map_data("county")  # get basic map data for all USA counties 
oh = subset(usa, region == "ohio")  # subset to counties in Ohio 
# names(oh)



p1 = ggplot() +
  geom_polygon(data = oh,
               aes(x = long, y = lat),
               fill = "white",
               color = "black") +
  ggtitle("a")  # bad

p2 = ggplot() +
  geom_polygon(
    data = oh,
    aes(x = long, y = lat,
        group = group),
    fill = "white",
    color = "black") +
  ggtitle("b")  # a basic map

p3 = ggplot() +
  geom_polygon(
    data = oh,
    aes(x = long, y = lat,
        group = group),
    fill = "white",
    color = "black"
  ) +
  coord_fixed(1.3) +
  ggtitle("c")  # a basic map

p4 = ggplot() + 
  geom_polygon(
  data = oh,
  aes(
    x = long,
    y = lat,
    group = group,
    fill = subregion
  ),
  color = "black",
  alpha = 0.3
) +
  coord_fixed(1.3) + guides(fill = FALSE) + 
  ggtitle("d")  # a colored map 

library(stringr)
oh$county = str_to_title(oh$subregion)

# load("./data/latlongs.RData")

library(ggrepel)

ggplot() +
  geom_polygon(
    data = oh,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "gray"
  ) +
  coord_fixed(1.3) +
  # geom_label_repel(data = latlongs,
  #                  aes(x = lon, y = lat, label = Campus),
  #                  color = "darkblue") +
  # geom_point(
  #   data = latlongs,
  #   aes(x = lon, y = lat),
  #   size = 1,
  #   color = "red",
  #   alpha = 0.5
  # ) +
  theme_map()

ggplot() +
  geom_polygon(
    data = oh,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "gray"
  ) +
  coord_fixed(1.3) +
  # geom_text_repel(data = latlongs,
  #                 aes(x = lon, y = lat, label = Campus),
  #                 color = "darkblue") +
  # geom_point(
  #   data = latlongs,
  #   aes(x = lon, y = lat),
  #   size = 1,
  #   color = "red",
  #   alpha = 0.5
  # ) +
  theme_map()

library(sp)
getLabelPoint <- # Returns a county-named list of label points
  function(county) {Polygon(county[c('long', 'lat')])@labpt}
centroids = by(oh, oh$county, getLabelPoint)     # Returns list
centroids2 <- do.call("rbind.data.frame", centroids)  # Convert to Data Frame
centroids2$county = rownames(centroids)
names(centroids2) <- c('clong', 'clat', "county")                 # Appropriate Header

ggplot() +
  geom_polygon(
    data = oh,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "gray"
  ) +
  coord_fixed(1.3)  +
  geom_text(
    data = centroids2,
    aes(x = clong, y = clat, label = county),
    color = "darkblue",
    size = 2.25
  )  +
  theme_map()

library(readxl)
acpovertyOH = read_excel("./data/acpovertyOH.xlsx", sheet = "counties")
colnames(acpovertyOH) = c("ranking", "county", "child1216", "child0711", 
                          "all1216", "all0711")
my.df = merge(oh, acpovertyOH[, c(2:3)], by = "county", all.x = TRUE, 
              sort = FALSE)

my.df = my.df[order(my.df$order), ]

ggplot() + geom_polygon(data = my.df, aes(x = long, y = lat, 
                                          group = group, fill = child1216), color = "black") + coord_fixed(1.3) + 
  geom_text(data = centroids2, aes(x = clong, y = clat, label = county), 
            color = "black", size = 2.25) + scale_fill_distiller(palette = "Spectral") + 
  labs(fill = "Child Poverty %") + theme_map()

ApplyQuintiles <- function(x) {
  cut(x, breaks = c(quantile(my.df$child1216, probs = seq(0, 
                                                          1, by = 0.2))), labels = c("0-20", "20-40", "40-60", 
                                                                                     "60-80", "80-100"), include.lowest = TRUE)
}

my.df$grouped_poverty <- sapply(my.df$child1216, ApplyQuintiles)

ggplot() + geom_polygon(data = my.df, aes(x = long, y = lat, 
                                          group = group, fill = grouped_poverty), color = "black") + 
  coord_fixed(1.3) + geom_text(data = centroids2, aes(x = clong, 
                                                      y = clat, label = county), color = "white", size = 2.25) + 
  scale_fill_brewer(palette = "Set1", direction = -1) + labs(fill = "Poverty Quntiles") + 
  theme_map()


