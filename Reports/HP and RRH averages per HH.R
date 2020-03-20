source(here("02_QPR_EEs.R"))

# EM asked for the average cost per household in 2019 for HP and RRH each

ReportStart <- "01012019"
ReportEnd <- "12312019"

qpr_spending <- Services %>%
  left_join(Enrollment,
            by = c("EnrollmentID", "PersonalID",
                   "ServiceProvider" = "ProjectName")) %>%
  left_join(smallProject, by = c("ProjectID", "ProjectType")) %>%
  select(
    PersonalID,
    OrganizationName,
    ProjectName,
    ProjectRegion,
    ProjectType,
    Amount,
    Description,
    RelationshipToHoH,
    ServiceStartDate,
    EntryDate,
    MoveInDateAdjust,
    ExitDate
  ) %>% 
  filter((ProjectType == 13 & !is.na(MoveInDateAdjust) | 
            ProjectType == 12) &
           RelationshipToHoH == 1 &
           !PersonalID %in% c(214315, 232008) & # typos in the data
           !is.na(Amount) &
           served_between(., ReportStart, ReportEnd)) %>%
  select(-RelationshipToHoH)


qpr_spending_averages <- qpr_spending %>%
  group_by(PersonalID, ProjectType) %>%
  summarise(HouseholdCost = sum(Amount)) %>%
  ungroup() %>%
  group_by(ProjectType) %>%
  summarise(AverageCostPerHH = mean(HouseholdCost))

ptc <- 13

ggplot(qpr_spending %>%
         filter(ProjectType == ptc) %>%
         group_by(PersonalID, ProjectType) %>%
         summarise(HouseholdCost = sum(Amount)), 
       aes(x = PersonalID, y = HouseholdCost)) +
  geom_point() +
  geom_hline(data = qpr_spending_averages %>%
               filter(ProjectType == ptc), aes(yintercept = AverageCostPerHH))
