#
## Proj task 1 Download Data
#

# Libraries
library(tidyverse)
library(magrittr)
library(sf)
library(tigris)

options(timeout = 300)


## Walkability Index

# Download walkability index zip
wakability <- download.file(url = "https://edg.epa.gov/EPADataCommons/public/OA/WalkabilityIndex.zip", 
                            destfile = "../Data/Raw/walkability.zip")

# Unzip file
unzip(zipfile = "../Data/Raw/walkability.zip",
      exdir = "../Data/Raw/walkability")


# Read in downloaded data
walkability <- st_read("../Data/Raw/walkability/Natl_WI.gdb")


## Low / moderate income households

# Read in from ArcGIS REST Services.
low_mod_income <- read_sf("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/LOW_MOD_INCOME_BY_BG/FeatureServer/0/query?where=State%20%3D%20'37'%20OR%20County%20%3D%20'001'%20OR%20County%20%3D%20'037'%20OR%20County%20%3D%20'063'%20OR%20County%20%3D%20'135'%20OR%20County%20%3D%20'183'&outFields=*&outSR=4326&f=json")

# Write
write_sf(low_mod_income,
         dsn = "../Data/Raw/low_mod_income.gpkg")

## NC Counties

# Read in with tigris
nc_counties <- counties(state = "NC")

# Write
write_sf(nc_counties,
         dsn = "../Data/Raw/nc_counties.gpkg")
