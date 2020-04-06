# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

load("images/COHHIOHMIS.RData")

# this is based on the ReportWriter reports we wrote for users to combine
# HMIS data. Confirmed by EM.

y <- Project %>%
  filter(ProjectID %in% c(718, 719, 721, 
                          1353, 1354, 
                          746, 747, 
                          1774, 15,
                          737, 738, 739,
                          548, 763, 764, 774,
                          1323, 208)) %>%
  mutate(
    AltProjectID = case_when(
      ProjectID %in% c(718, 719, 721) ~ 3000,
      ProjectID %in% c(1353, 1354) ~ 3001,
      ProjectID %in% c(746, 747) ~ 3002,
      ProjectID %in% c(1774, 15) ~ 3003,
      ProjectID %in% c(737, 738, 739) ~ 3004,
      ProjectID %in% c(548, 763, 764, 774) ~ 3005,
      ProjectID %in% c(1323, 208) ~ 3006
    ),
    AltProjectName = case_when(
      ProjectID %in% c(718, 719, 721) ~ "Butler SPC Combined",
      ProjectID %in% c(1353, 1354) ~ "Clark SPC Combined",
      ProjectID %in% c(746, 747) ~ "Jefferson PSH Combined",
      ProjectID %in% c(1774, 15) ~ "GLCAP PSH Combined",
      ProjectID %in% c(737, 738, 739) ~ "Lake PSH Combined",
      ProjectID %in% c(548, 763, 764, 774) ~ "Trumbull PSH Combined",
      ProjectID %in% c(1323, 208) ~ "Warren PSH Combined"
    )
  ) %>%
  select(ProjectID, ProjectName, AltProjectID, AltProjectName)


