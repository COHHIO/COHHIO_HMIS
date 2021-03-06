

dq_summary_for_a_project <- dq_project_summary %>%
  filter(ProjectName == "Adams - Adams County Shelter for the Homeless - ES")

library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

# Create data
data <- dq_summary_for_a_project %>% select(Type, Issue, Clients, "text" = Guidance)
  
# Generate the layout
packing <- circleProgressiveLayout(data$Clients, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p <- ggplot() +
  geom_polygon_interactive(
    data = dat.gg,
    aes(
      x,
      y,
      group = id,
      fill = id,
      tooltip = data$text[id],
      data_id = id
    ),
    colour = "black",
    alpha = 0.6
  ) +
  scale_fill_viridis() +
  geom_text(
    data = data,
    aes(x, y,
        label = gsub("Group_", "", Issue)),
    size = 2,
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  coord_equal()

# Turn it interactive
widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
# widg

# save the widget
# library(htmlwidgets)
# saveWidget(widg, file=paste0( getwd(), "/HtmlWidget/circular_packing_interactive.html"))

library(treemap)

# Create data
group <- c(rep("Awaiting Housing in RRH or PSH",4),rep("Referred to RRH or PSH",2),
           rep("No Housing Plan",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)

# Custom labels:
treemap(data, 
        index=c("group","subgroup"),   
        palette = "YlGnBu",
        vSize="value", type="index",
        fontsize.labels=c(15,12), # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"), # Color of labels
        fontface.labels=c(2,1), # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"), # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ), # Where to place labels in the rectangle?
        overlap.labels=0.5, # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F, # If true, labels are bigger when rectangle is bigger.
        
)





