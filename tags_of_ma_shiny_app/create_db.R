# Load the required libraries
library(DBI)
library(RSQLite)

# 1. Define the database file name
db_file <- "graffiti.sqlite"

# 2. Connect to the database.
# This command creates the file if it doesn't exist yet.
con <- dbConnect(RSQLite::SQLite(), db_file)

# 3. Define the table structure using a SQL command
# This creates a table called 'graffiti_tags' with all the columns we need.
# INTEGER PRIMARY KEY AUTOINCREMENT means each new entry gets a unique, automatic ID.
create_table_query <- "
CREATE TABLE IF NOT EXISTS graffiti_tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  latitude REAL NOT NULL,
  longitude REAL NOT NULL,
  tag_name TEXT,
  image_url TEXT
);"

# 4. Execute the command to create the table
dbExecute(con, create_table_query)

# 5. Disconnect from the database
dbDisconnect(con)

# Print a success message to your console
cat("Success! Database '", db_file, "' and table 'graffiti_tags' are set up.\n", sep = "")