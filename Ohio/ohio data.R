library(tidyverse)
library(readr)
# library(tidycensus)
# 
# Sys.getenv("CENSUS_API_KEY")
# 
# vars10 <- c("P005003", "P005004", "P005006", "P004003")
# 
# oh <- get_acs(geography = "county", variables = "B02001", year = 2017, 
#               state = "OH") %>%
#   mutate(pct = 100 * (value / summary_value))
# 
# ggplot(oh, aes(fill = pct, color = pct)) +
#   geom_sf() +
#   facet_wrap(~variable)
# 
# varsf1 <- load_variables(2010, "sf1")

# I'm using the following dataset for the analysis here:
#https://www.census.gov/data/tables/2017/demo/popest/counties-detail.html#ds


x <-
  read_csv("data/cc-est2017-alldata-39.csv",
           col_types =
             "cncccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn")

# colnames(x)
# consult the following document for what this dataset is all about:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2017/cc-est2017-alldata.pdf

# x$YEAR %>% unique() # this data can be used to compare year over year, yay

# we know this is Ohio data at the County level
x <- x %>% select(-SUMLEV, -STATE, -STNAME) %>%
  mutate(AGEGRP = case_when(
    AGEGRP == 0 ~ "Total",
    AGEGRP == 1 ~ "4",
    AGEGRP == 2 ~ "9",
    AGEGRP == 3 ~ "14",
    AGEGRP == 4 ~ "19",
    AGEGRP == 5 ~ "24",
    AGEGRP == 6 ~ "29",
    AGEGRP == 7 ~ "34",
    AGEGRP == 8 ~ "39",
    AGEGRP == 9 ~ "44",
    AGEGRP == 10 ~ "49",
    AGEGRP == 11 ~ "54",
    AGEGRP == 12 ~ "59",
    AGEGRP == 13 ~ "64",
    AGEGRP == 14 ~ "69",
    AGEGRP == 15 ~ "74",
    AGEGRP == 16 ~ "79",
    AGEGRP == 17 ~ "84",
    AGEGRP == 18 ~ "85+"
  ))

# need to create another table where gender is disaggregated from race
y <- x %>%
  mutate(
    WhiteAlone = NHWA_MALE + NHWA_FEMALE,
    BlackAlone = NHBA_MALE + NHBA_FEMALE,
    AIAlone = NHIA_MALE + NHIA_FEMALE,
    AsianAlone = NHAA_MALE + NHAA_FEMALE,
    NHAlone = NHNA_MALE + NHNA_FEMALE,
    Multi = NHTOM_MALE + NHTOM_FEMALE,
    Latino = H_MALE + H_FEMALE,
    STATE = "ohio",
    CTYNAME = tolower(str_remove(CTYNAME, " County"))
    ) %>%
  select(COUNTY, CTYNAME, STATE, YEAR, AGEGRP, TOT_POP, TOT_FEMALE, TOT_MALE, WhiteAlone,
         BlackAlone, AIAlone, AsianAlone, NHAlone, Multi, Latino)

AllAges <- y %>% filter(AGEGRP == "Total") %>% select(-AGEGRP)
Babies <- y %>% filter(AGEGRP == "4") %>% select(-AGEGRP)
ZeroTo19 <- y %>% filter(AGEGRP %in% c("4", "9", "14", "19")) %>%
  group_by(STATE, COUNTY, CTYNAME, YEAR) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
To24 <- y %>% filter(AGEGRP == "24") %>% select(-AGEGRP)
To34 <- y %>% filter(AGEGRP %in% c("29", "34")) %>%
  group_by(STATE, COUNTY, CTYNAME, YEAR) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
To49 <- y %>% filter(AGEGRP %in% c("39", "44", "49")) %>%
  group_by(STATE, COUNTY, CTYNAME, YEAR) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
To64 <- y %>% filter(AGEGRP %in% c("54", "59", "64")) %>%
  group_by(STATE, COUNTY, CTYNAME, YEAR) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
Over64 <- y %>% filter(AGEGRP %in% c("69", "74", "79", "84", "85+")) %>%
  group_by(STATE, COUNTY, CTYNAME, YEAR) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

getData <- function(agebracket, year){
  filter(agebracket, YEAR == year - 2007)
}

AllAges2010 <- getData(AllAges, 2010)
AllAges2017 <- getData(AllAges, 2017)

ohio <- map_data("county") %>%
  filter(region == "ohio")

z <- left_join(AllAges2017, ohio, 
                         by = c("CTYNAME" = "subregion")) 







