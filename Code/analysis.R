#
## Proj task 3 Analysis
#

### 

## Libraries
library(tidyverse)
library(magrittr)
library(sf)
library(tmap)
library(spdep)
library(spatialreg)

## Read in data
tri_walk_LMI <- read_sf("../Data/Cleaned/tri_walk_LMI.gpkg")

nc_counties <- read_sf("../Data/Cleaned/nc_counties.gpkg")

tri_counties <- read_sf("../Data/Cleaned/tri_counties.gpkg")


#### Goal: Check how walkability is related to
#### LMI (low - mod income populations) after accounting for density


### Run preliminary analysis

## Investigate clustering in variables (not necessary, but good for exploration)

# Create neighborhood object (queen order 1)
tri_q1nb <- poly2nb(tri_walk_LMI,
                    queen = TRUE)

# Summary
tri_q1nb


## Map of Queen 1 connections

# Blank map
plot(tri_walk_LMI %>% st_geometry(),
     col = "gray85",
     border = "gray65")

# Add neighborhood connections
plot(tri_q1nb, 
     st_centroid(tri_walk_LMI) %>% st_geometry(), 
     pch = 16, 
     col = "red", 
     lwd = 1, 
     add = TRUE)


# Create Row Standardized weight matrix
tri_q1nb_rs <- nb2listw(tri_q1nb,
                        style = "W",
                        zero.policy = TRUE)


# Moran's I tests
walk_moran <- moran.test(tri_walk_LMI$WALK_INDEX,
                        listw = tri_q1nb_rs)
LMI_moran <- moran.test(tri_walk_LMI$low_mod_pct,
                         listw = tri_q1nb_rs)
popdens_moran <- moran.test(tri_walk_LMI$POP_DENS_KM2,
                        listw = tri_q1nb_rs)

# Get summaries
walk_moran
LMI_moran
popdens_moran

# Strong clustering on on all variables, strong P val (expected)
# Is this still true after controlling for pop density?


## Walkability ~ density lin reg, investigate residuals.

# Regression test
walk_dens <- lm(WALK_INDEX ~ POP_DENS_KM2, data = tri_walk_LMI)
summary(walk_dens)

# Significant relationship (expected)

# Map residuals
tri_walk_LMI$walk_dens_resid <- residuals(walk_dens)

tm_shape(tri_walk_LMI) +
tm_polygons(
  fill = "walk_dens_resid",
  fill.scale = 
    tm_scale_intervals(
      n = 5,
      values = "brewer.rd_bu",
      style = "jenks"
    ),
  col = "black",
  col_alpha = 0.2,
  fill.legend = 
    tm_legend(
      title = "residual",
      position = tm_pos_in("right", "top")
    )
) +
tm_layout(frame = FALSE) +
tm_title("Resids (walk ~ pop dens)", position = tm_pos_out("center", "top"))

# Residuals appear clustered.


## Check moran's I on resids

# Moran's I on resid
resid_moran <- moran.test(tri_walk_LMI$walk_dens_resid,
                          listw = tri_q1nb_rs)

# Get summary
resid_moran

## Clustering appears smaller than before, but still notable and significant.


## Check for clustering after also accounting for LMI (just in case)

# Full regression test
walk_dens_LMI <- lm(WALK_INDEX ~ POP_DENS_KM2 + low_mod_pct, data = tri_walk_LMI)

summary(walk_dens_LMI)
# The good news: There is a significant relationship, even controlling for population density.
# The bad news: There are probably still spatial factors, so this is
#               invalid until we can rule that out.


# Map residuals
tri_walk_LMI$walk_dens_LMI_resid <- residuals(walk_dens_LMI)

tm_shape(tri_walk_LMI) +
  tm_polygons(
    fill = "walk_dens_LMI_resid",
    fill.scale = 
      tm_scale_intervals(
        n = 5,
        values = "brewer.rd_bu",
        style = "jenks"
      ),
    col = "black",
    col_alpha = 0.2,
    fill.legend = 
      tm_legend(
        title = "residual",
        position = tm_pos_in("right", "top")
      )
  ) +
  tm_layout(frame = FALSE) +
  tm_title("Resids (walk ~ pop dens + LMI)", position = tm_pos_out("center", "top"))


# Moran's I on full resid
full_resid_moran <- moran.test(tri_walk_LMI$walk_dens_LMI_resid,
                               listw = tri_q1nb_rs)
full_resid_moran

# As expected, clustering is still present. We will need to control
# for this with a spatial lag model.


### Spatial lag vs error

## Use Lagrange Multiplier test ----
lagrange <- lm.RStests(model = walk_dens_LMI, 
                       listw = tri_q1nb_rs,
                       test = "all")

summary(lagrange)

# Adjusted LM models favor spatial lag model.
# SARMA is also significant, so it's worth running.


### Running the model

# Spatial Lag Model ----
sp_lag_mod <- lagsarlm(WALK_INDEX ~ POP_DENS_KM2 + low_mod_pct,
                       data = tri_walk_LMI,
                       listw = tri_q1nb_rs,
                       zero.policy = TRUE)

summary(sp_lag_mod)

# Test shows significance in both pop density AND low mod pct.
# Z val is higher in population density, but low mod pct p val is still far below 0.05.

# Issue: while a lot of spatial influence was addressed,
# there is still residual autocorrelation (p val of 1.8331e-06).


# SARMA Spatial Autoregressive Moving Average Model ----
sarma_mod <- sacsarlm(WALK_INDEX ~ POP_DENS_KM2 + low_mod_pct,
                      data = tri_walk_LMI,
                      listw = tri_q1nb_rs,
                      type = "sac",
                      zero.policy = TRUE)

summary(sarma_mod)

# Spatial factor has been better accounted for. AIC has decreased further
# and significance, though slightly reduced, is still there.


## Must check residuals for clustering.

# Add sac resids
tri_walk_LMI$sac_resid <- residuals(sarma_mod)

# Moran's I on residuals
sac_resid_moran <- moran.test(tri_walk_LMI$sac_resid,
                              listw = tri_q1nb_rs,
                              zero.policy = TRUE)
sac_resid_moran

# Clustering is no longer significant under SARMA model.
# This means the model has successfully accounted for spatial factors
# and the significant results are meaningful.

# Map shows lack of spatial pattern.
tm_shape(tri_walk_LMI) +
  tm_polygons(
    fill = "sac_resid",
    fill.scale = 
      tm_scale_intervals(
        n = 5,
        values = "brewer.rd_bu",
        style = "jenks"
      ),
    col = "black",
    col_alpha = 0.2,
    fill.legend = 
      tm_legend(
        title = "residual",
        position = tm_pos_in("right", "top")
      )
  ) +
  tm_layout(frame = FALSE) +
  tm_title("SARMA Model Resids", position = tm_pos_out("center", "top"))

