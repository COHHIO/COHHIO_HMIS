library(ggplot2)
library(lubridate)

source("get_SPM_data.R")

metric_3 <- spm_3_homeless_count %>%
  filter(Type == "Unduplicated Total Sheltered Homeless Persons") %>%
  pivot_longer(cols = c("Prior", "Current")) %>%
  mutate(Year = case_when(
    name == "Prior" ~ paste(format.Date(spm_prior_start_date, "%b %Y"), "to",
                            format.Date(spm_prior_end_date - days(1), "%b %Y")),
    name == "Current" ~ paste(format.Date(spm_current_start_date, "%b %Y"), "to",
                              format.Date(spm_current_end_date - days(1), "%b %Y"))
  )) %>%
  select("Measure" = Type, "LiterallyHomeless" = value, Year)

ggplot(metric_3) +
  geom_col(aes(Year, LiterallyHomeless)) +
  labs(title =
         "Metric 3: Homeless Clients Served in Shelter or Transitional Housing") +
  xlab("") +
  ylab("Homeless Clients in HMIS") +
  theme_bw() 


