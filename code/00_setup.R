### Forest disturbance stack v2

### This script creates required folders in directory
### Matt Bitters
### matthew.bitters@colorado.edu


# Set cyverse working directory
setwd("/home/jovyan/data-store/forest_disturbance_stack_v2")

# Install and load here package for folder management
if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Set up necessary directories
dir.create("data", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# Create subfolders inside 'data'
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/derived", showWarnings = FALSE)
dir.create("data/output", showWarnings = FALSE)

# Create downloaded subfolder inside of raw (to manually save data b/c script 01 
# not automatically downloading data)
dir.create("data/raw/downloaded-files", showWarnings = FALSE)
