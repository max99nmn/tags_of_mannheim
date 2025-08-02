# Load necessary libraries
# You might need to install exifr first: install.packages("exifr")
library(exifr)
library(dplyr)
library(DBI)
library(RSQLite)
library(purrr)
library(stringr)

# --- Step 1: Define Your Image Folder ---

# IMPORTANT: Change this path to the folder where your pictures are stored.
image_folder <- "www/"

# --- Step 2: Read EXIF Data ---

# Get a list of all JPG/JPEG files in that folder
image_file_paths <- list.files(image_folder, pattern = "\\.jpg", full.names = TRUE, recursive = TRUE)

if (length(image_file_paths) == 0) {
  stop("No image files found in the specified folder. Check the 'image_folder' path.")
}

# Read the GPS and Date information from all images at once
# exifr is very efficient and does this in one go!
exif_data <- read_exif(image_file_paths, tags = c("GPSLatitude", "GPSLongitude", "DateTimeOriginal"))

tag_names <- map_chr(basename(image_file_paths), ~ str_split_1(basename(.x), pattern = "\\.| ")[1])  
# --- Step 3: Clean the Data for the Database ---

# Filter for photos that actually have GPS data and select/rename columns
# to match what your Shiny app expects.
locations_df <- exif_data |> 
  filter(!is.na(GPSLatitude) & !is.na(GPSLongitude)) |> 
  select(
    latitude = GPSLatitude,
    longitude = GPSLongitude,
    image_url = SourceFile
  ) |> 
  # Add the placeholder columns your Shiny app needs
  mutate(
    tag_name = tag_names,
    image_url = basename(image_url)
  )

if (nrow(locations_df) == 0) {
  stop("Found images, but none of them contained GPS location data.")
}

# --- Step 4: Write to the SQLite Database ---

# This is the same database file your Shiny app will use
db_file <- "graffiti.sqlite"

# Connect to the database. This will create the file if it doesn't exist.
con <- dbConnect(RSQLite::SQLite(), db_file)

# Write the data frame to a table named 'graffiti_tags'
# overwrite = TRUE is good for the first time you run this.
# For future imports, you might change it to append = TRUE.
dbWriteTable(con, "graffiti_tags", locations_df, overwrite = TRUE)

# Disconnect from the database
dbDisconnect(con)

# Print a success message to the console
cat("Success! Wrote", nrow(locations_df), "locations to the database:", db_file, "\n")