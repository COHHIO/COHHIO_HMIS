library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit,
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Offers,
   Organization, ProjectCoC, Scores, Services, Users)

Veterans <- Client %>%
  filter(VeteranStatus == 1) %>%
  select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican, NativeHIOtherPacific,
         White, RaceNone, Ethnicity, Gender)

VeteranHHs <- Veterans %>%
  left_join(Enrollment, by = "PersonalID") %>%
  select(PersonalID, EnrollmentID, ProjectID, EntryDate, HouseholdID, 
         RelationshipToHoH, LivingSituation, LengthOfStay, LOSUnderThreshold,
         PreviousStreetESSH, DateToStreetESSH, TimesHomelessPastThreeYears,
         MonthsHomelessPastThreeYears, DisablingCondition, DateOfEngagement,
         MoveInDate, VAMCStation, CountyServed, CountyPrior, ExitDate, 
         Destination, OtherDestination, ExitAdjust, AgeAtEntry)

VeteranHHs <- Project %>%
  select(ProjectID, OrganizationName, OperatingStartDate, OperatingEndDate,
         ProjectType, GrantType, ProjectName, ProjectAKA, RegionName, Region) %>%
  right_join(VeteranHHs, by = "ProjectID")

VeteranHHs <- VeteranHHs %>%
  left_join(VeteranCE, by = c("PersonalID" = "ClientID"))


CurrentVeterans <- VeteranHHs %>%
  filter((ProjectType %in% c(1, 2, 4, 8, 12) & (
    ymd(EntryDate) <= today() &
      (is.na(ExitDate) | ymd(ExitDate) > today())
  )) |
    (ProjectType %in% c(3, 9, 13) & (
      ymd(MoveInDate) <= today() &
        (is.na(ExitDate) |
           ymd(ExitDate) > today())
    )))

CurrentVeteranCounts <- CurrentVeterans %>%
  filter(ProjectType %in% c(1, 2, 4, 8)) %>%
  group_by(ProjectName, RegionName) %>%
  summarise(Veterans = n()) %>%
  ungroup()

VeteransWithHousingPlan <- CurrentVeterans %>%
  filter(!is.na(PHTrack) & ymd(ExpectedPHDate) >= today())

WithHousingPlanCount <- VeteransWithHousingPlan %>%
  group_by(ProjectName, RegionName)  %>%
  summarise(HasHousingPlan = n()) %>%
  ungroup()

VeteransCurrentHousingOffers <- CurrentVeterans %>%
  filter(!is.na(MostRecentOfferStatus) &
               ymd(MostRecentOfferDate) > today() - 14
           )

WithHousingOffersCount <- VeteransCurrentHousingOffers %>%
  group_by(ProjectName, RegionName) %>%
  summarise(HasCurrentOffer = n()) %>%
  ungroup()

CurrentVeteranCounts <- CurrentVeteranCounts %>%
  left_join(WithHousingOffersCount, by = c("ProjectName", "RegionName")) %>%
  left_join(WithHousingPlanCount, by = c("ProjectName", "RegionName"))

#replace all the NAs with zeros
CurrentVeteranCounts[is.na(CurrentVeteranCounts)] <- 0

plotVetsData <- CurrentVeteranCounts %>% gather(key = "CountType", value = "Counts",
                                -ProjectName, -RegionName, -Veterans)

rm(Client, Enrollment, Inventory, Project, Regions, VeteranCE, Veterans, 
   VeteransCurrentHousingOffers, VeteransWithHousingPlan, WithHousingOffersCount,
   WithHousingPlanCount)

ggplot(plotVetsData, aes(x = ProjectName, fill = CountType)) +
  geom_col(aes(y = Veterans)) +
  geom_col(aes(y = Counts))

save.image("images/Veterans.RData")

