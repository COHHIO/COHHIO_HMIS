# .images_to_copy <- c(
#   "Utilization"
#   , "QPR_SPDATs"
#   , "QPR_EEs"
#   , "Veterans"
#   , "Data_Quality"
#   , "SPM_data"
#   , "ProjectEvaluation"
#   , "Active_List"
# )
# 
# .dir <- c("../Rminor/data", "../Rminor_elevated/data", "../COHHIO_HMIS/images")
# 
# purrr::walk(.dir, ~ {
#   .d <- .x
#   if (!dir.exists(.d)) {
#     dir.create(.d)
#   }
#   purrr::walk(paste0("images/", .images_to_copy, ".RData"), ~ {
#     file.copy(.x, overwrite = TRUE, fs::path(.d, basename(.x)))
#   })
# }) 


directory_where_all_your_projects_live <-
  "C:\\Users\\HMIS\\Documents\\R\\"

project_and_folder_that_contains_image <- "COHHIO_HMIS\\images\\"

image_filename <- c(
  "Data_Quality.RData",
  "ProjectEvaluation.RData",
  "Utilization.RData",
  "QPR_SPDATs.RData",
  "QPR_EEs.RData",
  "Veterans.RData",
  "SPM_data.RData",
  "Active_List.RData"
)

project_and_folder_you_wish_had_the_image <- "Rminor\\data\\"

file.link(from = paste0(
  directory_where_all_your_projects_live,
  project_and_folder_that_contains_image, 
  image_filename),
  to = paste0(
    directory_where_all_your_projects_live,
    project_and_folder_you_wish_had_the_image, 
    image_filename
  ))
