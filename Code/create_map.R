#
## Proj task 1 Create Map
#

## Libraries
library(tidyverse)
library(magrittr)
library(sf)
library(tmap)
library(classInt)


# Read in data
tri_walk_LMI <- read_sf("../Data/Cleaned/tri_walk_LMI.gpkg")

nc_counties <- read_sf("../Data/Cleaned/nc_counties.gpkg")

tri_counties <- read_sf("../Data/Cleaned/tri_counties.gpkg")


## Walkability

# Create bounding box for map (give a pad)
map_bbox <- st_bbox(tri_walk_LMI)
map_bbox["xmin"] <- map_bbox["xmin"] - 0.5
map_bbox["xmax"] <- map_bbox["xmax"] + 0.5
map_bbox["ymin"] <- map_bbox["ymin"] - 0.3
map_bbox["ymax"] <- map_bbox["ymax"] + 0.3


# Map
tri_walkability_map <-
  tm_shape(nc_counties, bbox = map_bbox) +
  tm_fill() +
  tm_borders(lwd = 1) +
  tm_shape(tri_walk_LMI) +
  tm_polygons(
    fill = "WALK_INDEX",
    fill.scale = tm_scale_intervals(
      breaks = c(1, 5.76, 10.51, 15.26, 20), # scale adjusted to fit standard low, below average, above average, high categories.
      values = "brewer.gn_bu"
      ),
    col = "black",
    col_alpha = 0.2,
    fill.legend = 
      tm_legend(
        title = "Walkability Index",
        position = tm_pos_in("right", "top")
      )
  ) +
  tm_shape(tri_counties) +
  tm_borders(lwd = 2) +
  tm_layout(frame = TRUE,
            frame.lwd = 3) +
  tm_scalebar(
    breaks = c(0,5,10,15,20),
    text.size = 0.8, 
    position = c(0.75, 0.16)
  ) +
  tm_compass(
    size = 4,
    position = c(0.91, 0.35)
  ) +
  tm_credits(
    "Author: Joshua Mark",
    position = c(0.74, 0.07),
    size = 1
  ) +
  tm_title("Walkability in the NC Triangle", position = tm_pos_out("center", "top"))

tri_walkability_map

# Save to file
tmap_save(tri_walkability_map,
          filename = "../Graphics/tri_walkability_map.png",
          width = 1000,
          height = 800,
          dpi = 144)


## Low - Mod Income %

# Map
low_mod_income_map <-
  tm_shape(nc_counties, bbox = map_bbox) +
  tm_fill() +
  tm_borders(lwd = 1) +
  tm_shape(tri_walk_LMI) +
  tm_polygons(
    fill = "low_mod_pct",
    fill.scale = 
      tm_scale_intervals(
        n = 5,
        values = "brewer.gn_bu",
        style = "jenks"
      ),
    col = "black",
    col_alpha = 0.2,
    fill.legend = 
      tm_legend(
        title = "Low - Mod %",
        position = tm_pos_in("right", "top")
      )
  ) +
  tm_shape(tri_counties) +
  tm_borders(lwd = 2.5) +
  tm_layout(frame = TRUE,
            frame.lwd = 3) +
  tm_scalebar(
    breaks = c(0,5,10,15,20),
    text.size = 0.8, 
    position = c(0.75, 0.16)
  ) +
  tm_compass(
    size = 4,
    position = c(0.91, 0.35) 
  ) +
  tm_credits(
    "Author: Joshua Mark",
    position = c(0.74, 0.07),
    size = 1
  ) +
  tm_title("Low - Moderate Income Households in the NC Triangle", position = tm_pos_out("center", "top"))

low_mod_income_map


# Save to file
tmap_save(low_mod_income_map,
          filename = "../Graphics/low_mod_income_map.png",
          width = 1000,
          height = 800,
          dpi = 144)


### Bivariate map

# Calculate natural breaks
walk_breaks <- classIntervals(tri_walk_LMI$WALK_INDEX, 
                              n = 3, 
                              style = "fisher")
LMI_breaks <- classIntervals(tri_walk_LMI$low_mod_pct, 
                             n = 3, 
                             style = "fisher")

walk_breaks$brks
LMI_breaks$brks

# Map
bivar_map <- 
  tm_shape(nc_counties, bbox = map_bbox) +
    tm_fill() +
    tm_borders(lwd = 1) +
    tm_shape(tri_walk_LMI) +
    tm_fill(
      fill = 
        tm_vars(
          c("WALK_INDEX", "low_mod_pct"), 
          multivariate = TRUE
        ),
      fill.scale = 
        tm_scale_bivariate(
          scale1 = 
            tm_scale_intervals(
              breaks = walk_breaks$brks,
              labels = c("L", "M", "H")),
          scale2 = 
            tm_scale_intervals(
              breaks = LMI_breaks$brks,
              labels = c("L", "M", "H")),
          values = "bu_br_bivs"),
      fill.legend =
        tm_legend_bivariate(
          xlab = "LMI %",
          ylab = "Walkability",
          frame = FALSE,
          position = tm_pos_in("right", "top")
        )) +
    tm_borders(col = "black",
               col_alpha = 0.3) +
    tm_shape(tri_counties) +
    tm_borders(lwd = 2.5) +
    tm_layout(frame = TRUE,
              frame.lwd = 3) +
    tm_scalebar(
      breaks = c(0,5,10,15,20),
      text.size = 0.8,
      position = c(0.75, 0.16)
    ) +
    tm_compass(
      size = 4,
      position = c(0.91, 0.35)
    ) +
    tm_credits(
      "Author: Joshua Mark",
      position = c(0.74, 0.07),
      size = 1
    ) +
    tm_title("Walkability vs. Low - Mod Income % in the NC Triangle", position = tm_pos_out("center", "top"))

bivar_map

# Save to file
tmap_save(bivar_map,
          filename = "../Graphics/bivar_map.png",
          width = 1000,
          height = 800,
          dpi = 144)
