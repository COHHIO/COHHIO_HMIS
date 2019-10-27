# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at
#<https://www.gnu.org/licenses/>.

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0700 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0700 -)")),
              "SPM_data/0700a.csv")
  
  file.remove(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0700 -)")))
}




file.rename(paste0("SPM_data/", 
                   list.files("./SPM_data", pattern = "(0700.1b)")),
            "SPM_data/0700b.csv")
file.rename(paste0("SPM_data/", 
                   list.files("./SPM_data", pattern = "(0701)")),
            "SPM_data/0701.csv")
file.rename(paste0("SPM_data/", 
                   list.files("./SPM_data", pattern = "(0702)")),
            "SPM_data/0702.csv")
file.rename(paste0("SPM_data/", 
                   list.files("./SPM_data", pattern = "(0703)")),
            "SPM_data/0703.csv")
file.rename(paste0("SPM_data/", 
                   list.files("./SPM_data", pattern = "(0704)")),
            "SPM_data/0704.csv")
file.rename(paste0("SPM_data/", 
                   list.files("./SPM_data", pattern = "(0706)")),
            "SPM_data/0706.csv")
