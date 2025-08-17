#the image pairs in the original and the thumbnail folder must be named equally
#don't know why Sus images still show no lng and lat info in the exif even if they are visible in google photos...
fill_db_with_content <- function(
  pool_con,
  original_images_folder
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

  #dummy values
  date_added <- base::format(base::Sys.time(), "%Y:%m:%d %H:%M:%S")
  added_by <- "user123"

  #a random number (from normal distribution) is added to the exact lng and lat from the exif and then rounded to 4 decimal places
  all_info <- exif_of_orig_img |>
    dplyr::mutate(
      file_name = base::basename(SourceFile),
      image_url = paste0("original_images/", file_name),
      thumbnail_url = paste0(
        "thumbnail_images/",
        stringr::str_replace(file_name, "\\.\\w+", ".jpg")
      ),
      tag_name = stringr::str_replace(file_name, " .+|\\..+", ""),
      date_added = date_added,
      added_by = added_by
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
      date_added,
      added_by
    ) |>
    tidyr::drop_na(lng, lat, tag_name)

  #split all_info tibble into db tables and then write them to the db
  #Users table
  users_tibble <- all_info |>
    dplyr::select(user_name = added_by) |>
    dplyr::distinct() |>
    dplyr::mutate(pw_hash = "abcde", salt = "abcde")

  DBI::dbWriteTable(pool_con, "Users", users_tibble, append = TRUE)

  #Tags table
  tags_tibble <- all_info |>
    dplyr::select(tag_name) |>
    dplyr::distinct() |>
    dplyr::mutate(tag_id = 1:dplyr::n(), .before = tag_name)

  DBI::dbWriteTable(pool_con, "Tags", tags_tibble, append = TRUE)

  #Images table
  images_tibble <- all_info |>
    dplyr::select(image_url, thumbnail_url, date_created) |>
    dplyr::mutate(image_id = 1:dplyr::n(), .before = image_url)

  DBI::dbWriteTable(pool_con, "Images", images_tibble, append = TRUE)

  #Locations table
  locations_tibble <- all_info |>
    dplyr::left_join(
      tags_tibble |> dplyr::select(tag_id, tag_name),
      by = "tag_name"
    ) |>
    dplyr::left_join(
      images_tibble |> dplyr::select(image_id, image_url, date_created),
      by = dplyr::join_by("image_url", "date_created")
    ) |>
    dplyr::select(image_id, tag_id, added_by, lng, lat, date_added)

  DBI::dbWriteTable(pool_con, "Locations", locations_tibble, append = TRUE)
}

pool_con <- open_db_pool("inst/extdata/tom_database.sqlite")
create_db(pool_con)
fill_db_with_content(
  pool_con,
  "C:/github/tags_of_mannheim/inst/app/www/original_images"
)
DBI::dbGetQuery(pool_con, "SELECT * FROM Users")
DBI::dbGetQuery(pool_con, "SELECT * FROM Tags")
DBI::dbGetQuery(pool_con, "SELECT * FROM Images")
DBI::dbGetQuery(pool_con, "SELECT * FROM Locations")
pool::poolClose(pool_con)
