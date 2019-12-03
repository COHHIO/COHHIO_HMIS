library(treemap)
library(d3treeR)

# dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)

dq_summary_errors <- dq_project_summary %>%
  filter(Type == "Error")

# basic treemap
p <- treemap(
  dq_summary_errors %>% filter(OrganizationName == "Adams County Shelter for the Homeless, Inc."),
  index = c("ProjectName", "Issue"),
  vSize = "Clients",
  type = "index",
  palette = "Set2",
  bg.labels = c("gray"),
  align.labels = list(c("center", "top"),
                      c("right", "bottom"))
)            

# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2( p ,  rootname = "Data Quality Errors" )

# save the widget
# library(htmlwidgets)
# saveWidget(inter, file=paste0( getwd(), "/HtmlWidget/interactiveTreemap.html"))