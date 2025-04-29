### Forest disturbance stack v2

### This script filters and rasterizes Welty & Jeffries fire data from 2019-2020
# across the Western United States.
### Matt Bitters
### matthew.bitters@colorado.edu


# Set cyverse working directory
setwd("/home/jovyan/data-store/forest_disturbance_stack_v2")

# Install and load required packages
packages <- c("here", "sf", "dplyr", "ggplot2", "tigris", "terra", "httr", "progress")
installed <- packages %in% installed.packages()[,"Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load packages
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(tigris)
library(terra)
library(httr)
library(progress) 


### Read in data

# Save path
gdb_wj__path <- here("data", "raw", "downloaded-files", "Fire_Feature_Data.gdb")

# Welty & Jeffries layers
st_layers(gdb_wj__path)

# Read combined dataset layer
wjdat <- st_read(dsn = gdb_wj__path, layer =  "USGS_Wildland_Fire_Combined_Dataset")

# Look at fields within layer
names(wjdat)

# Look at CRS
st_crs(wjdat)

# Re-project CRS to EPSG 5070
wjdat <- st_transform(wjdat, crs = 5070)
st_crs(wjdat)




### Filtering (1999-2020 and western states)


# Filter fires to just include those between 1999 and 2020
wjdat <- wjdat %>%
  filter(between(Fire_Year, 1999, 2020))

# How many fires?
nrow(wjdat)
# 80327 total fires

# Filter to just wildfire and related (i.e., remove rx fire)
# Look at options
sort(unique(wjdat$Assigned_Fire_Type))
table(wjdat$Assigned_Fire_Type)

wjdat <- wjdat %>%
  filter(Assigned_Fire_Type %in% c("Likely Wildfire", 
                                   "Unknown - Likely Wildfire",
                                   "Wildfire"))

# How many fires?
nrow(wjdat)
# 49967 total fires now

# Filter to just western US states

# Read in state boundaries
# states_sf <- states(cb = TRUE, resolution = "20m", year = 2020) # tigris not working in cyverse.
# Download manually and read in using line below.
states_sf <- st_read(here("data", "raw", "downloaded-files", "cb_2020_us_state_20m", "cb_2020_us_state_20m.shp"))

states_sf <- st_transform(states_sf, st_crs(wjdat))  # Match CRS

# Filter to western states
western_states <- c("California", "Oregon", "Washington", "Idaho", "Montana",
                    "Nevada", "Utah", "Arizona", "Colorado", "New Mexico", "Wyoming")

west_sf <- states_sf %>%
  filter(NAME %in% western_states)

# Check geometries
table(st_geometry_type(wjdat))
# MULTISURFACE giving issues

# Keep only POLYGON and MULTIPOLYGON geometries (drops 1 MULTISURFACE)
wjdat <- wjdat %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

# Make valid geometries
wjdat <- st_make_valid(wjdat)
wjdat <- st_cast(wjdat, "MULTIPOLYGON")

# Spatial intersection (only include fires within western states)
wj_west <- st_intersection(wjdat, west_sf)

# How many fires?
nrow(wj_west)
# 32312 total fires now




### Plot to make sure data look reasonable. It does!

# Color by year
ggplot() +
  geom_sf(data = wj_west, aes(fill = Fire_Year), color = NA, alpha = 0.7) +
  geom_sf(data = west_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", trans = "log", name = "Year") +  
  labs(title = "Burned Areas in the Western US (1999–2020)") +
  theme_minimal()




###############################################################################




### Rasterization Setup


# Convert to terra SpatVector
wj_vect <- vect(wj_west)

# Create raster template
r_template <- rast(ext(wj_vect), resolution = 30)  # 30-meter resolution
crs(r_template) <- crs(wj_vect)

# Unique years
fire_years <- sort(unique(wj_vect$Fire_Year))

# Initialize rasters
latest_fire_raster <- rast(r_template)
burn_count_raster <- rast(r_template)
values(burn_count_raster) <- 0  # Initialize with zeros

# Rasterize annual fires 
# Initialize progress bar
pb <- progress_bar$new(
  total = length(fire_years),
  format = "  Processing [:bar] :percent (Year :current/:total - :message)",
  clear = FALSE,
  width = 60
)

for (i in seq_along(fire_years)) {
  year <- fire_years[i]
  pb$tick(tokens = list(message = year))
  
  fire_year_vect <- wj_vect[wj_vect$Fire_Year == year, ]
  fire_year_vect$burn_value <- year
  
  fire_raster <- rasterize(fire_year_vect, r_template, field = "burn_value", touches = TRUE)
  
  # Update latest fire year
  latest_fire_raster <- cover(fire_raster, latest_fire_raster)
  
  # Update burn frequency
  burned_cells <- !is.na(fire_raster)
  burn_count_raster[burned_cells] <- burn_count_raster[burned_cells] + 1
}

# Save rasters
dir.create(here("data", "derived", "welty-jeffries-raster"), showWarnings = FALSE, recursive = TRUE)

writeRaster(latest_fire_raster, here("data", "derived", "welty-jeffries-raster", "latest_fire_year.tif"), overwrite = TRUE)
writeRaster(burn_count_raster, here("data", "derived", "welty-jeffries-raster", "burn_frequency.tif"), overwrite = TRUE)

message("Done! Rasters saved to /data/derived"/"welty-jeffriess-raster/")


