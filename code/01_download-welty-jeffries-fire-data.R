################## WARNING ################## 

# This script not downloading and unzipping geodatabases properly as of
# 4-25-2025. Download files manually from:
# https://www.sciencebase.gov/catalog/item/61aa537dd34eb622f699df81

################## WARNING ################## 


### Forest disturbance stack v2

### This script downloads Welty & Jeffries fire data from 2019-2020 across the
### Western United States.
### Matt Bitters
### matthew.bitters@colorado.edu


# Install and load required packages
packages <- c("httr", "jsonlite")
installed <- packages %in% installed.packages()[,"Package"]
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load packages
library(httr)
library(jsonlite)
library(here)

# Set ScienceBase item ID
item_id <- "61aa537dd34eb622f699df81"
api_url <- paste0("https://www.sciencebase.gov/catalog/item/", item_id, "?format=json")

# Fetch metadata and download associated files
response <- GET(api_url)

if (status_code(response) == 200) {
  item_data <- content(response, as = "parsed", type = "application/json")
  files <- item_data$files
  
  if (length(files) > 0) {
    dir.create("data/raw/downloaded-files", recursive = TRUE, showWarnings = FALSE)
    
    pb <- txtProgressBar(min = 0, max = length(files), style = 3)
    
    for (i in seq_along(files)) {
      file <- files[[i]]
      file_url <- file$url
      file_name <- file$name
      dest_path <- file.path("data/raw/downloaded-files", file_name)
      
      if (!file.exists(dest_path)) {
        download.file(file_url, destfile = dest_path, mode = "wb", quiet = TRUE)
        message("Downloaded: ", file_name)
      } else {
        message("Already exists: ", file_name, " — skipping download.")
      }
      setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    # Handle zip files
    zip_files <- list.files("data/raw/downloaded-files", pattern = "\\.zip$", full.names = TRUE)
    
    for (zip_path in zip_files) {
      zip_name <- basename(zip_path)
      zip_list <- tryCatch(unzip(zip_path, list = TRUE), error = function(e) NULL)
      
      if (is.null(zip_list) || all(endsWith(zip_list$Name, "/"))) {
        warning("⚠️  Skipping broken or empty zip: ", zip_name)
        next
      }
      
      # Unzip to folder named after zip (minus extension)
      unzip_to <- file.path("data/raw/downloaded-files", tools::file_path_sans_ext(zip_name))
      dir.create(unzip_to, showWarnings = FALSE)
      
      tryCatch({
        unzip(zip_path, exdir = unzip_to)
        file.remove(zip_path)
        message("✅ Unzipped and cleaned up: ", zip_name)
      }, error = function(e) {
        warning("❌ Failed to unzip: ", zip_name, " — ", e$message)
      })
    }
    
  } else {
    message("No downloadable files found.")
  }
  
} else {
  message("Failed to retrieve metadata. Status code: ", status_code(response))
}
