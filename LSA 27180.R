projectx <- ClientUtilizers %>% filter(ProjectID == 878)

projectx <- projectx %>% mutate(daysinproject = difftime(ExitAdjust, EntryAdjust, units = "days"))

projectx %>% group_by(ProjectID) %>% summarise(avg = mean(daysinproject))

projectxSingles <- projectx %>% filter(grepl("s_", HouseholdID))

projectxSingles %>% group_by(ProjectID) %>% summarise(avg = mean(daysinproject))

