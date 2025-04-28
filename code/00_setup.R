### Forest disturbance stack v2

### This script creates required folders in directory
### Matt Bitters
### matthew.bitters@colorado.edu

# Install and load here package for folder management
if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Set up necessary directories
dir.create(here("data"), showWarnings = FALSE)
dir.create(here("figures"), showWarnings = FALSE)

# Create subfolders inside 'data'
dir.create(here("data", "raw"), showWarnings = FALSE)
dir.create(here("data", "derived"), showWarnings = FALSE)
dir.create(here("data", "output"), showWarnings = FALSE)
