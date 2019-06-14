library(tidyverse)
library(tidycensus)

Sys.getenv("CENSUS_API_KEY")

vars10 <- c("P005003", "P005004", "P005006", "P004003")

oh <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "OH", geometry = TRUE) %>%
  mutate(pct = 100 * (value / summary_value))

ggplot(oh, aes(fill = pct, color = pct)) +
  geom_sf() +
  facet_wrap(~variable)

varsf1 <- load_variables(2010, "sf1")
