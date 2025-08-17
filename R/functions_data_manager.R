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
      tag_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      tag_name TEXT NOT NULL UNIQUE,
      tag_info TEXT
    ) STRICT;
  "
  )

  DBI::dbExecute(
    pool_con,
    "
    CREATE TABLE Images (
      image_id      INTEGER PRIMARY KEY AUTOINCREMENT,
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
      loc_id     INTEGER PRIMARY KEY AUTOINCREMENT,
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
      color
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
    "SELECT * FROM Locations WHERE tag_id IN (",
    placeholders,
    ");"
  )

  locations_for_map <- DBI::dbGetQuery(
    pool_con,
    sql_query,
    params = base::list(tag_ids)
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

#the image pairs in the original and the thumbnail folder must be named equally
#don't know why Sus images still show no lng and lat info in the exif even if they are visible in google photos...
fill_db_with_content <- function(
  pool_con,
  original_images_folder,
  thumbnail_images_folder
) {
  #get all original_images paths
  orig_img_paths <- base::list.files(
    path = original_images_folder,
    full.names = TRUE
  )
  exif_of_orig_img <- exifr::read_exif(
    orig_img_paths,
    tags = c("GPSLatitude", "GPSLongitude", "DateTimeOriginal")
  )

  date_added <- base::format(base::Sys.time(), "%Y:%m:%d %H:%M:%S")

  #a random number (from normal distribution) is added to the exact lng and lat from the exif and then rounded to 4 decimal places
  all_info <- exif_of_orig_img |>
    dplyr::mutate(
      file_name = base::basename(SourceFile),
      image_url = paste0("original_images/", file_name),
      thumbnail_url = paste0(
        "thumbnail_images/",
        stringr::str_replace(file_name, "\\.\\w+", ".jpg")
      ),
      tag_name = stringr::str_replace(file_name, " .+", ""),
      date_added = date_added
    ) |>
    dplyr::rename(dplyr::all_of(c(
      lat = "GPSLatitude",
      lng = "GPSLongitude",
      date_created = "DateTimeOriginal"
    ))) |>
    dplyr::mutate(dplyr::across(
      c(lng, lat),
      ~ base::round(.x + stats::rnorm(1, sd = 0.00085), digits = 4)
    )) |>
    dplyr::select(
      image_url,
      thumbnail_url,
      tag_name,
      lat,
      lng,
      date_created,
      date_added
    ) |>
    tidyr::drop_na(lng, lat, tag_name)

  #tbd(one): split all_info tibble into db tables and then write them to the db
  #DBI::dbWriteTable(pool_con, "Tags", tags_tibble, overwrite = TRUE)
}

# pool_con <- open_db_pool_connection("inst/extdata/tom_database.sqlite")
# create_db(pool_con)
# loc_db_name <- query_db(pool_con, 1)
# DBI::dbDispool_connect(pool_con)
