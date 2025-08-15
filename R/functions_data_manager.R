open_db_connection <- function(db_file_path) {
  DBI::dbConnect(RSQLite::SQLite(), db_file_path)
}

create_db <- function(con) {
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  DBI::dbExecute(
    con,
    "
    CREATE TABLE Users (
      user_name TEXT PRIMARY KEY,
      pw_hash   TEXT NOT NULL,
      salt      TEXT NOT NULL
    ) STRICT;
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE Tags (
      tag_id   INTEGER PRIMARY KEY,
      tag_name TEXT NOT NULL UNIQUE,
      tag_info TEXT
    ) STRICT;
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE Images (
      image_id      INTEGER PRIMARY KEY,
      image_url     TEXT NOT NULL UNIQUE,
      thumbnail_url TEXT NOT NULL UNIQUE,
      date_created  TEXT
    ) STRICT;
  "
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE Locations (
      loc_id     INTEGER PRIMARY KEY,
      image_id   INTEGER,
      tag_id     INTEGER,
      added_by   TEXT,
      lng        REAL NOT NULL,
      lat        REAL NOT NULL,
      date_added TEXT NOT NULL,
      rating     INTEGER,
      
      FOREIGN KEY (image_id) REFERENCES Images(image_id),
      FOREIGN KEY (tag_id) REFERENCES Tags(tag_id),
      FOREIGN KEY (added_by) REFERENCES Users(user_name)
    ) STRICT;
  "
  )
}

query_locations <- function(con, tag_ids) {
  #when e.g. no tag is selected then give an empty tibble in the right format
  if (is.null(tag_ids)) {
    locations_table_col_names <- DBI::dbGetQuery(
      con,
      "PRAGMA table_info(Locations);"
    )$name

    #currently not very good as it must be changed when changes to Locations table are made
    location_table_col_types <- list(
      integer(0),
      integer(0),
      integer(0),
      character(0),
      double(0),
      double(0),
      character(0),
      integer(0)
    )

    col_definitions <- rlang::set_names(
      location_table_col_types,
      locations_table_col_names
    )

    locations_tibble <- tibble::tibble(!!!col_definitions)
    return(locations_tibble)
  }

  placeholders <- base::paste(
    rep("?", times = base::length(tag_ids)),
    collapse = ","
  )

  sql_query <- base::paste(
    "SELECT * FROM Locations WHERE tag_id IN (",
    placeholders,
    ");"
  )

  locations <- DBI::dbGetQuery(con, sql_query, params = base::list(tag_ids))

  return(locations)
}

# con <- open_db_connection("inst/extdata/tom_database.sqlite")
# create_db(con)
# loc_db_name <- query_db(con, 1)
# DBI::dbDisconnect(con)
