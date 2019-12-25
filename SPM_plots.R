library(ggplot2)
library(lubridate)

source("get_SPM_data.R")

metric_3 <- spm_3_homeless_count %>%
  filter(Type == "Unduplicated Total Sheltered Homeless Persons") %>%
  pivot_longer(cols = c("Prior", "Current")) %>%
  mutate(Year = case_when(
    name == "Prior" ~ ymd(spm_prior_end_date),
    name == "Current" ~ ymd(spm_current_end_date)
  )) %>%
  select("Measure" = Type, "LiterallyHomeless" = value, Year)

ggplot(metric_3) +
  geom_line(aes(Year, LiterallyHomeless)) +
  geom_point(aes(Year, LiterallyHomeless)) +
  ylim(0, 15000) +
  labs(title =
         "Metric 3: Homeless Clients Served in Shelter or Transitional Housing",
       caption = "These numbers do not include clients who may have experienced
       unsheltered homelessness or who were at non-HMIS-participating 
       emergency shelters.") +
  xlab("") +
  ylab("Homeless Clients in HMIS") +
  scale_x_date(
    date_labels = "%Y",
    breaks = c(spm_prior_end_date, spm_current_end_date),
    minor_breaks = NULL,
    limits = c(
      spm_prior_end_date - months(4),
      spm_current_end_date + months(4)
    )
  ) +
  theme_bw() 

