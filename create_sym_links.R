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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
#<https://www.gnu.org/licenses/>.

# HOW TO SET UP YOUR SYMBOLIC LINKS ON YOUR SYSTEM (Windows-specific)
# command prompt As Administrator, then:
# mklink "C:\Users\HMIS1\Documents\R\Rminor\data\Data_Quality.RData" 
# "C:\Users\HMIS1\Documents\R\COHHIO_HMIS\images\Data_Quality.RData"
# obviously replace these paths with what's on your PC. It's basically:
# mklink "where you want to add the link" "where you want the link to point"

r_directory <- "insert the path to your R directory here"

DataQualityImageLocation <- paste0(r_directory, 
                                   "\R\COHHIO_HMIS\images\Data_Quality.RData")
DataQualityImageRmTarget <- paste0(r_directory, 
                                   "\R\Rminor\data\Data_Quality.RData")
DataQualityImageRmeTarget <- paste0(r_directory, 
                                    "\R\Rminor_elevated\data\Data_Quality.RData")

shell(sprintf("mklink /H %s %s", 
              normalizePath(DataQualityImageRmTarget, mustWork = FALSE),
              normalizePath(DataQualityImageLocation)
))
shell(sprintf("mklink /H %s %s", 
              normalizePath(DataQualityImageRmeTarget, mustWork = FALSE),
              normalizePath(DataQualityImageLocation)
))


QPR_EEsImageLocation <- paste0(r_directory, 
                               "\R\COHHIO_HMIS\images\QPR_EEs.RData")
QPR_EEsImageRmTarget <- paste0(r_directory, 
                               "\R\Rminor\data\QPR_EEs.RData")
QPR_EEsImageRmeTarget <- paste0(r_directory, 
                                "\R\Rminor_elevated\data\QPR_EEs.RData")

shell(sprintf("mklink /H %s %s", 
              normalizePath(QPR_EEsImageRmTarget, mustWork = FALSE),
              normalizePath(QPR_EEsImageLocation)
))
shell(sprintf("mklink /H %s %s", 
              normalizePath(QPR_EEsImageRmeTarget, mustWork = FALSE),
              normalizePath(QPR_EEsImageLocation)
))


QPR_SPDATImageLocation <- paste0(r_directory, 
                                 "\R\COHHIO_HMIS\images\QPR_SPDAT.RData")
QPR_SPDATsImageRmTarget <- paste0(r_directory, 
                                  "\R\Rminor\data\QPR_SPDAT.RData")
QPR_SPDATImageRmeTarget <- paste0(r_directory, 
                                  "\R\Rminor_elevated\data\QPR_SPDAT.RData")

shell(sprintf("mklink /H %s %s", 
              normalizePath(QPR_SPDATsImageRmTarget, mustWork = FALSE),
              normalizePath(QPR_SPDATImageLocation)
))
shell(sprintf("mklink /H %s %s", 
              normalizePath(QPR_SPDATImageRmeTarget, mustWork = FALSE),
              normalizePath(QPR_SPDATImageLocation)
))

UtilizationImageLocation <- paste0(r_directory, 
                                   "\R\COHHIO_HMIS\images\Utilization.RData")
UtilizationImageRmTarget <- paste0(r_directory, 
                                   "\R\Rminor\data\Utilization.RData")
UtilizationImageRmeTarget <- paste0(r_directory, 
                                    "\R\Rminor_elevated\data\Utilization.RData")

shell(sprintf("mklink /H %s %s", 
              normalizePath(UtilizationImageRmTarget, mustWork = FALSE),
              normalizePath(UtilizationImageLocation)
))
shell(sprintf("mklink /H %s %s", 
              normalizePath(UtilizationImageRmeTarget, mustWork = FALSE),
              normalizePath(UtilizationImageLocation)
))

VeteransImageLocation <- paste0(r_directory, 
                                "\R\COHHIO_HMIS\images\Veterans.RData")
VeteransImageRmTarget <- paste0(r_directory, 
                                "\R\Rminor\data\Veterans.RData")
VeteransImageRmeTarget <- paste0(r_directory, 
                                 "\R\Rminor_elevated\data\Veterans.RData")

shell(sprintf("mklink /H %s %s", 
              normalizePath(VeteransImageRmTarget, mustWork = FALSE),
              normalizePath(VeteransImageLocation)
))
shell(sprintf("mklink /H %s %s", 
              normalizePath(VeteransImageRmeTarget, mustWork = FALSE),
              normalizePath(VeteransImageLocation)
))
