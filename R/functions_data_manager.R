open_db_pool <- function(db_file_path) {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = db_file_path
  )
}

create_db <- function(pool_con) {
  DBI::dbExecute(pool_con, "PRAGMA foreign_keys = ON;")

  DBI::dbExecute(
    pool_con,
    "
    CREATE TABLE Users (
      user_name TEXT PRIMARY KEY,
      pw_hash   TEXT NOT NULL,
      salt      TEXT NOT NULL
    ) STRICT;
  "
  )

  DBI::dbExecute(
    pool_con,
    "
    CREATE TABLE Tags (
      tag_id   INTEGER PRIMARY KEY,
      tag_name TEXT NOT NULL UNIQUE,
      tag_info TEXT
    ) STRICT;
  "
  )

  DBI::dbExecute(
    pool_con,
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
    pool_con,
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

query_locations_for_map <- function(pool_con, tag_ids, color_palette) {
  #when e.g. no tag is selected then give an empty tibble in the right format
  if (is.null(tag_ids)) {
    locations_table_col_names <- c(
      DBI::dbGetQuery(
        pool_con,
        "PRAGMA table_info(Locations);"
      )$name,
      "tag_name",
      "color"
    )

    #currently not very good as it must be changed when changes to Locations table are made
    location_table_col_types <- list(
      integer(0),
      integer(0),
      integer(0),
      character(0),
      double(0),
      double(0),
      character(0),
      integer(0),
      character(0),
      character(0)
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
    "SELECT L.*, T.tag_name
     FROM Locations AS L
     LEFT JOIN Tags AS T ON L.tag_id = T.tag_id
     WHERE L.tag_id IN (",
    placeholders,
    ");"
  )

  locations_for_map <- DBI::dbGetQuery(
    pool_con,
    sql_query,
    params = tag_ids
  ) |>
    tibble::as_tibble()

  color_lut <- tibble::tibble(
    tag_id = tag_ids,
    color = color_palette[1:base::length(tag_ids)]
  )

  locations_for_map <- locations_for_map |>
    dplyr::left_join(color_lut, by = "tag_id")

  locations_for_map
}

get_locations_for_list <- function(all_data, map_bounds) {
  locations_for_list <- all_data |>
    dplyr::filter(
      lat < map_bounds$north,
      lat > map_bounds$south,
      lng < map_bounds$east,
      lng > map_bounds$west
    )

  locations_for_list
}
