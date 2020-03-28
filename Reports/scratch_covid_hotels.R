
library(tidyverse)
library(readxl)
library(here)

hotels <- read_xlsx(here("data/OHLAhotels.xlsx")) %>%
  select("PropertyName" = 1, 
         "Address" = 2,
         "City" = 3,
         "State" = 4,
         "ZIP" = 5,
         "County" = 6,
         "ContactName" = 7,
         Email,
         Phone,
         "AltPhone" = 10,
         "Notes" = 11)

how_many_per_county <- hotels %>%
  group_by(County) %>%
  summarise(n())


bucketing1 <- hotels %>%
  mutate(
    Region = case_when(
      County %in% c(
        "Adams",
        "Brown",
        "Butler",
        "Champaign",
        "Clark",
        "Clermont",
        "Clinton",
        "Darke",
        "Fayette",
        "Greene",
        "Hamilton",
        "Highland",
        "Miami",
        "Montgomery",
        "Preble",
        "Warren"
      ) ~ "Southwest",
      County %in% c(
        "Delaware",
        "Fairfield",
        "Franklin",
        "Knox",
        "Licking",
        "Logan",
        "Madison",
        "Marion",
        "Morrow",
        "Pickaway",
        "Union"
      ) ~ "Central",
      County %in% c(
        "Allen",
        "Auglaize",
        "Richland",
        "Crawford",
        "Defiance",
        "Erie",
        "Fulton",
        "Hancock",
        "Hardin",
        "Henry",
        "Huron",
        "Lucas",
        "Mercer", 
        "Ottawa",
        "Paulding",
        "Putnam",
        "Sandusky",
        "Seneca",
        "Ashland",
        "Shelby",
        "Van Wert",
        "Williams",
        "Wood",
        "Wyandot"
      ) ~ "Northwest",
      County %in% c(
        "Ashtabula",
        "Columbiana",
        "Cuyahoga",
        "Geauga",
        "Holmes",
        "Lake",
        "Lorain",
        "Mahoning",
        "Medina",
        "Portage",
        "Stark",
        "Summit",
        "Trumbull",
        "Wayne"
      ) ~ "Northeast",
      County %in% c(
        "Athens",
        "Harrison",
        "Belmont",
        "Coshocton",
        "Carroll",
        "Gallia",
        "Guernsey",
        "Tuscarawas",
        "Hocking",
        "Jefferson",
        "Jackson",
        "Lawrence",
        "Miegs",
        "Monroe",
        "Morgan",
        "Muskingum",
        "Noble",
        "Perry",
        "Pike",
        "Ross",
        "Scioto",
        "Vinton",
        "Washington"
      ) ~ "Southeast"
    )
  )

how_many_per_bucket1 <- bucketing1 %>%
  group_by(Region) %>%
  summarise(n())


entitlements <- tribble(
  ~ County, ~ Region,
  "Montgomery", "Dayton, Kettering/Montgomery County CoC",
  "Hamilton", "Cincinnati/Hamilton County CoC",
  "Lucas", "Toledo/Lucas County CoC",
  "Cuyahoga", "Cleveland/Cuyahoga County CoC",
  "Franklin", "Columbus/Franklin County CoC",
  "Mahoning", "Youngstown/Mahoning County CoC",
  "Summit", "Akron, Barberton/Summit County CoC",
  "Stark", "Canton, Massillon, Alliance/Stark County CoC"
)

regions <- read_csv(here("data/Regions.csv")) %>%
  mutate(Region = paste("Balance of State CoC Region", Region))

regions <- rbind(entitlements, regions)

bucketing2 <- hotels %>%
  left_join(regions, by = "County")

how_many_per_bucket2 <- bucketing2 %>%
  group_by(Region) %>%
  summarise(n())


