.images_to_copy <- c(
  "Utilization"
  , "QPR_SPDATs"
  , "QPR_EEs"
  , "Veterans"
  , "Data_Quality"
  , "SPM_data"
  , "ProjectEvaluation"
  , "Active_List"
)

.dir <- c("../Rminor/data", "../Rminor_elevated/data")

purrr::walk(.dir, ~ {
  .d <- .x
  if (!dir.exists(.d)) {
    dir.create(.d)
  }
  purrr::walk(paste0("images/", .images_to_copy, ".RData"), ~ {
    file.copy(.x, overwrite = TRUE, fs::path(.d, basename(.x)))
  })
}) 
