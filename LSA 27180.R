# run the Bed Unit Utilization script down to the point where it creates the 
# ClientUtilizers object

projectx <- ClientUtilizers %>% filter(ProjectID == 1017)

projectx <- projectx %>% 
  mutate(daysinproject = difftime(ExitAdjust, EntryAdjust, units = "days"))

projectx %>% group_by(ProjectID) %>% summarise(avg = mean(daysinproject))

projectxSingles <- projectx %>% filter(grepl("s_", HouseholdID))

projectxSingles %>% group_by(ProjectID) %>% summarise(avg = mean(daysinproject))

projectxHHs <- projectx %>% filter(grepl("h_", HouseholdID))

projectxHHs %>% group_by(ProjectID) %>% summarise(avg = mean(daysinproject))

rm(projectx, projectxSingles, projectxHHs)
