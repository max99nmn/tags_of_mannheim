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
  if (is.null(tag_ids) | length(tag_ids) == 0) {
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
    "SELECT L.*, T.tag_name, I.thumbnail_url, I.image_url
     FROM Locations AS L
     LEFT JOIN Tags AS T ON L.tag_id = T.tag_id
     LEFT JOIN Images AS I ON L.image_id = I.image_id
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
    tag_id = base::as.numeric(tag_ids),
    color = color_palette[1:base::length(tag_ids)]
  )

  locations_for_map <- locations_for_map |>
    dplyr::left_join(color_lut, by = "tag_id")

  locations_for_map
}

query_data_for_selector <- function(pool_con) {
  sql_query <- "SELECT T.tag_id, T.tag_name FROM Tags as T"

  data_for_selector <- DBI::dbGetQuery(
    pool_con,
    sql_query
  ) |>
    tibble::as_tibble()

  data_for_selector
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

get_current_upload_image <- function(all_uploaded_images, index) {
  all_uploaded_images |>
    dplyr::slice(index)
}

ensure_admin_user <- function(pool_con) {
  admin_check <- DBI::dbGetQuery(
    pool_con,
    "SELECT 1 FROM Users WHERE user_name = 'ToM Admin'"
  )
  if (base::nrow(admin_check) == 0) {
    DBI::dbExecute(
      pool_con,
      "INSERT INTO Users (user_name, pw_hash, salt) VALUES ('ToM Admin', 'dummy', 'dummy')"
    )
  }
}

get_or_create_tag_id <- function(pool_con, tag_name) {
  existing_tag <- DBI::dbGetQuery(
    pool_con,
    "SELECT tag_id FROM Tags WHERE tag_name = ?",
    params = list(tag_name)
  )

  if (base::nrow(existing_tag) == 0) {
    DBI::dbExecute(
      pool_con,
      "INSERT INTO Tags (tag_name) VALUES (?)",
      params = list(tag_name)
    )
    return(DBI::dbGetQuery(pool_con, "SELECT last_insert_rowid()")[[1]])
  }

  existing_tag$tag_id[1]
}

add_image_placeholder <- function(pool_con, date_created) {
  DBI::dbExecute(
    pool_con,
    "INSERT INTO Images (image_url, thumbnail_url, date_created) VALUES ('temp', 'temp', ?)",
    params = list(date_created)
  )
  DBI::dbGetQuery(pool_con, "SELECT last_insert_rowid()")[[1]]
}

update_image_urls <- function(pool_con, img_id, image_url, thumbnail_url) {
  DBI::dbExecute(
    pool_con,
    "UPDATE Images SET image_url = ?, thumbnail_url = ? WHERE image_id = ?",
    params = list(image_url, thumbnail_url, img_id)
  )
}

save_location_entry <- function(pool_con, img_id, tag_id, lng, lat) {
  DBI::dbExecute(
    pool_con,
    "INSERT INTO Locations (image_id, tag_id, added_by, lng, lat, date_added) VALUES (?, ?, 'ToM Admin', ?, ?, ?)",
    params = list(img_id, tag_id, lng, lat, as.character(Sys.Date()))
  )
}
