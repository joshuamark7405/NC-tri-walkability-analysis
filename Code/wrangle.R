#
## Proj task 1 Data Wrangling
#

## Libraries
library(tidyverse)
library(magrittr)
library(sf)


## Read in data

# Read in walkability
walkability <- st_read("../Data/Raw/walkability/Natl_WI.gdb")

# Get counties
nc_counties <- st_read("../Data/Raw/nc_counties.gpkg")

# Read in low_mod_income
low_mod_income <- st_read("../Data/Raw/low_mod_income.gpkg")


## Walkability cleaning

# Take a look
glimpse(walkability)


# Filter/rename columns
walkability %<>% select("GEOID" = "GEOID20",
                        "STATE" = "STATEFP",
                        "COUNTY" = "COUNTYFP",
                        "POP" = "TotPop",
                        "WALK_INDEX" = "NatWalkInd",
                        "AREA" = "Shape_Area")


# Check for missing (none)
walkability_miss <- walkability |> filter(if_any("WALK_INDEX", is.na))


# Check for duplicates (none)
unique_obs_walk <- walkability$NatWalkInd |>
  unique() |>
  length()


# Check for valid geometries
valid_polys_walkability <- st_is_valid(walkability) %>% sum()


# Filter to triangle counties
tri_walkability <- walkability |>
  filter(COUNTY %in% c("001", "037", "063", "135", "183"),
         STATE %in% "37")

# Add pop density, change area from m^2 to km^2
tri_walkability %<>% mutate("POP_DENS_KM2" = POP / (AREA / 1000000))


## NC Counties cleaning


# Take a look
glimpse(nc_counties)


# Filter to Triangle Region
tri_counties <- nc_counties |>
  filter(COUNTYFP %in% c("001", "037", "063", "135", "183"))
                        

# Filter columns
nc_counties %<>% select("GEOID" = "GEOID")
tri_counties %<>% select("GEOID" = "GEOID")


# Check for missing (none) (since tri_counties is simply a clipped extent of nc_counties,
#                           I will only run tests on nc_counties.)
nc_counties_miss <- nc_counties |> filter(if_any("GEOID", is.na))


# Check for duplicates (none)
unique_obs_nc_counties <- nc_counties$GEOID |>
  unique() |>
  length()


# Check for valid geometries
valid_polys_counties <- st_is_valid(nc_counties) %>% sum()



## low_mod_income cleaning

# Take a look
glimpse(low_mod_income)


# Filter to triangle counties
tri_low_mod_income <- low_mod_income |>
  filter(County %in% c("001", "037", "063", "135", "183"),
         State %in% "37")

# Filter columns
tri_low_mod_income %<>% select("GEOID" = "GEOID",
                               "low_mod_pct" = "Lowmod_pct")


# Check for missing (nothing significant)
tri_low_mod_income_miss <- tri_low_mod_income |> filter(if_any("low_mod_pct", is.na))


# Check for duplicates
unique_obs_tri_low_mod_income <- tri_low_mod_income$GEOID |>
  unique() |>
  length()


# Check for valid geometries
valid_polys_tri_low_mod_income <- st_is_valid(tri_low_mod_income) %>% sum()



## Reproject to NC
nc_counties %<>% st_transform(crs = 32119)
tri_counties %<>% st_transform(crs = 32119)
tri_walkability %<>% st_transform(crs = 32119)
tri_low_mod_income %<>% st_transform(crs = 32119)


## Issue: LMI and walkability mismatched. Need to do spatial intersection to
## accumulate LMI to walkability level

# Intersect
intersected <- st_intersection(tri_walkability, tri_low_mod_income)

# Calculate area of overlap
intersected$area_overlap <- st_area(intersected)

# What is geometry column called?
names(intersected)

# Weight LMI by area 
intersected <- intersected %>%
  mutate(weighted_LMI = low_mod_pct * as.numeric(area_overlap)) %>%
  group_by(GEOID) %>%
  summarise(low_mod_pct = sum(weighted_LMI) / sum(as.numeric(area_overlap)),
            Shape = st_union(Shape))


## Join intersection LMI to walkability
tri_walk_LMI <- left_join(tri_walkability, st_drop_geometry(intersected), by = "GEOID")


### Write

## Write to cleaned
write_sf(tri_walk_LMI,
         dsn = "../Data/Cleaned/tri_walk_LMI.gpkg")

write_sf(nc_counties,
         dsn = "../Data/Cleaned/nc_counties.gpkg")

write_sf(tri_counties,
         dsn = "../Data/Cleaned/tri_counties.gpkg")


## Write to outputs (not used in analysis but good to have)
write_sf(tri_walkability,
         dsn = "../Data/Outputs/tri_walkability.gpkg")

write_sf(tri_low_mod_income,
         dsn = "../Data/Outputs/tri_low_mod_income.gpkg")
