project_levels <-  function(ProjectType) {
  case_when(
    ProjectType == 1 ~ "Emergency Shelters", 
    ProjectType == 2 ~ "Transitional Housing", 
    ProjectType == 3 ~ "Permanent Supportive Housing", 
    ProjectType == 4 ~ "Street Outreach", 
    ProjectType == 8 ~ "Safe Haven",
    ProjectType == 9 ~ "Permanent Supportive Housing", 
    ProjectType == 12 ~ "Prevention",  
    ProjectType == 13 ~ "Rapid Rehousing"
  )
}
