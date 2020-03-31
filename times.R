
start <- now()
source("00_get_the_CSV_things.R") # 27 secs 3/30/2020
end <- now()

rm(list = ls())

start <- now()
source("01_Bed_Unit_Utilization.R") # 17 secs 3/30/2020
end <- now()

end - start

rm(list = ls())

start <- now()
source("02_QPR_SPDATs.R") # 2.8 mins 3/30/2020
end <- now()
end - start

rm(list = ls())

start <- now()
source("02_QPR_EEs.R") # 9 secs 3/30/2020
end <- now()
end - start

rm(list = ls())

start <- now()
source("03_Veterans.R") # 3 secs 3/30/2020
end <- now()
end - start

rm(list = ls())


start <- now()
source("04_DataQuality.R") # 3.3 mins 3/30/2020
end <- now()
end - start

rm(list = ls())


start <- now()
source("05_Cohorts.R") # 14 secs 3/30/2020
end <- now()
end - start

rm(list = ls())

# before doing this one, you have to rerun all the scripts after clearing the
# environment, go into this script and tell it not to ditch "start"
start <- now()
source("06_Project_Evaluation.R") # 20 secs 3/30/2020
end <- now()
end - start

rm(list = ls())


start <- now()
source("07_SPMs.R") # 5 secs 3/30/2020
end <- now()
end - start

rm(list = ls())

# you have to go into this script and tell it not to ditch "start"
start <- now()
source("08_Active_List.R") # 5 secs 3/30/2020
end <- now()
end - start

rm(list = ls())
