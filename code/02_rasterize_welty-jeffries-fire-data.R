### Forest disturbance stack v2

### This script filters and rasterizes Welty & Jeffries fire data from 1984-2020
### across the Western United States.
### Matt Bitters
### matthew.bitters@colorado.edu


# Set cyverse memory max to avoid crashing
#terraOptions(memmax=256)

# Set cyverse working directory
#setwd("/home/jovyan/data-store/forest_disturbance_stack_v2")

# Install and load required packages
packages <- c("here", "sf", "dplyr", "ggplot2", "terra", "purrr", "progressr")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(purrr)
library(progressr)


# ==== READ AND FILTER DATA ====

# Path to geodatabase
gdb_fire__path <- here("data", "raw", "downloaded-files", "Fire_Feature_Data.gdb")

# Read combined fire dataset
firedat <- st_read(dsn = gdb_fire__path, layer = "USGS_Wildland_Fire_Combined_Dataset")

# Re-project to EPSG 5070
firedat <- st_transform(firedat, crs = 5070)

# Filter by year and fire type
firedat <- firedat %>%
  filter(between(Fire_Year, 1984, 2020)) %>%
  filter(Assigned_Fire_Type %in% c("Likely Wildfire", 
                                   "Unknown - Likely Wildfire",
                                   "Wildfire"))

# Read state boundaries (already downloaded)
states_sf <- st_read(here("data", "raw", "downloaded-files", "cb_2020_us_state_20m", "cb_2020_us_state_20m.shp"))
states_sf <- st_transform(states_sf, st_crs(firedat))

# Filter to western US states
western_states <- c("California", "Oregon", "Washington", "Idaho", "Montana",
                    "Nevada", "Utah", "Arizona", "Colorado", "New Mexico", "Wyoming")
west_sf <- states_sf %>%
  filter(NAME %in% western_states)

# Clean geometries
firedat <- firedat %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# Intersect with western states
firedat_west <- st_intersection(firedat, west_sf)


### Plot to make sure data look reasonable. It does!

# Color by year
ggplot() +
  geom_sf(data = firedat_west, aes(fill = Fire_Year), color = NA, alpha = 0.7) +
  geom_sf(data = west_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "magma", trans = "log", name = "Year") +  
  labs(title = "Burned Areas in the Western US (1984–2020)") +
  theme_minimal()



# ==== RASTERIZE BY YEAR AND SAVE ====

# Create template raster
template_raster <- rast(
  extent = ext(firedat_west),
  resolution = 30,  # Adjust as needed
  crs = st_crs(firedat_west)$wkt
)

# Split fire polygons by Fire_Year
fires_split <- split(firedat_west, firedat_west$Fire_Year)

# Set up progress bar
handlers(global = TRUE)
handlers("progress")

# Rasterize by year using purrr + progressr
with_progress({
  p <- progressor(along = fires_split)
  
  yearly_rasters <- imap(fires_split, function(sf_year, year) {
    p(sprintf("Rasterizing year %s", year))
    
    vect_year <- vect(sf_year)
    
    r <- terra::rasterize(vect_year, template_raster, field = "USGS_Assigned_ID", touches = TRUE)
    
    names(r) <- paste0("wildfire_id_", year)
    return(r)
  })
})

# Combine rasters into one stack
fire_stack <- rast(yearly_rasters)

# Check layer names
names(fire_stack)

# Manually reset names
names(fire_stack) <- c("wildfire_id_1984", "wildfire_id_1985", "wildfire_id_1986", "wildfire_id_1987",
                        "wildfire_id_1988", "wildfire_id_1989", "wildfire_id_1990", "wildfire_id_1991",
                        "wildfire_id_1992", "wildfire_id_1993", "wildfire_id_1994", "wildfire_id_1995",
                        "wildfire_id_1996", "wildfire_id_1997", "wildfire_id_1998", "wildfire_id_1999",
                        "wildfire_id_2000", "wildfire_id_2001", "wildfire_id_2002", "wildfire_id_2003",
                        "wildfire_id_2004", "wildfire_id_2005", "wildfire_id_2006", "wildfire_id_2007",
                        "wildfire_id_2008", "wildfire_id_2009", "wildfire_id_2010", "wildfire_id_2011",
                        "wildfire_id_2012", "wildfire_id_2013", "wildfire_id_2014", "wildfire_id_2015",
                        "wildfire_id_2016", "wildfire_id_2017", "wildfire_id_2018", "wildfire_id_2019",
                        "wildfire_id_2020")

# Check data type for compression when writing
datatype(fire_stack, bylyr = TRUE)

# Save output raster stack
output_path <- here("data", "derived", "wildfire_id.tif")
writeRaster(fire_stack, output_path, datatype = "FLT4S", overwrite = TRUE, gdal = c("COMPRESS=DEFLATE"))
message("Raster stack saved to: ", output_path)

# Load raster stack and check names
fire_stack_test <- rast(here("data", "derived", "wildfire_id.tif"))
names(fire_stack_test)
