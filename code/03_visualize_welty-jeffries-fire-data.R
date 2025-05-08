### Forest disturbance stack v2

### This script visualizes Welty & Jeffries fire data from 1984-2020
### across the Western United States by creating a gif. Note that this uses
### 1000m resolution because the 30m resolution raster is too large.
### Matt Bitters
### matthew.bitters@colorado.edu


# Install the necessary packages if they aren't already installed
packages <- c("here", "terra", "sf", "ggplot2", "magick", "dplyr")
installed <- packages %in% installed.packages()[,"Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load packages
library(here)
library(terra)
library(sf)
library(ggplot2)
library(magick)
library(dplyr)

# Read raster
r <- rast(here("data", "derived", "wildfire_id_1000.tif"))

# Read western states shapefile 
states_sf <- sf::st_read(here("data", "raw", "downloaded-files", "cb_2020_us_state_20m", "cb_2020_us_state_20m.shp"))

r_crs <- st_crs(r)  # Get the CRS from the raster as an sf object

states_sf <- st_transform(states_sf, crs = r_crs)

western_states <- c("California", "Oregon", "Washington", "Idaho", "Montana",
                    "Nevada", "Utah", "Arizona", "Colorado", "New Mexico", "Wyoming")
west_sf <- states_sf %>%
  filter(NAME %in% western_states) %>%
  st_transform(r_crs)  # Match raster CRS

  


# Prepare plot extent
ext <- ext(r)
bbox <- st_as_sfc(st_bbox(
  c(xmin = xmin(ext), xmax = xmax(ext),
    ymin = ymin(ext), ymax = ymax(ext)),
  crs = st_crs(r)
))

# Initialize list of frames
frames <- list()

# Loop through each year/layer
for (i in 1:nlyr(r)) {
  layer <- r[[i]]
  year_name <- names(r)[i]
  
  # Convert to polygon for fire areas (using terra then sf)
  fire_poly <- as.polygons(layer, dissolve = FALSE, na.rm = TRUE)
  fire_sf <- st_as_sf(fire_poly)
  fire_sf <- fire_sf[!is.na(fire_sf[[1]]), ]  # Keep only fire pixels
  
  # Plot with ggplot
  p <- ggplot() +
    geom_sf(data = west_sf, fill = "white", color = "black", linewidth = 0.4) +
    geom_sf(data = fire_sf, fill = "black", color = NA) +
    coord_sf(xlim = c(ext[1], ext[2]), ylim = c(ext[3], ext[4])) +
    labs(title = paste("Fires in", year_name)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 18))
  
  # Convert ggplot to image
  img <- image_graph(width = 800, height = 600, res = 96)
  print(p)
  dev.off()
  
  frames[[i]] <- img
}

# Combine into animated gif
gif <- image_animate(image_join(frames), fps = 1)
image_write(gif, here("data", "derived", "wildfire_id_1000_gif.gif"))

