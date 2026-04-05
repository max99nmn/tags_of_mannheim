process_and_save_upload <- function(
  pool_con,
  current_img,
  crop_data,
  map_center,
  tag_name,
  date_created
) {
  # 1. Tag und Placeholder-Bild in DB sichern
  tag_id <- get_or_create_tag_id(pool_con, tag_name)
  img_id <- add_image_placeholder(pool_con, date_created)

  # 2. Pfade definieren und Verzeichnisse sicherstellen
  orig_dir <- base::file.path("inst", "app", "www", "original_images")
  thumb_dir <- base::file.path("inst", "app", "www", "thumbnail_images")
  base::dir.create(orig_dir, showWarnings = FALSE, recursive = TRUE)
  base::dir.create(thumb_dir, showWarnings = FALSE, recursive = TRUE)

  orig_path <- base::file.path(orig_dir, base::paste0(img_id, ".jpg"))
  thumb_path <- base::file.path(thumb_dir, base::paste0(img_id, ".jpg"))

  # 3. Bildverarbeitung
  img <- magick::image_read(current_img$temp_datapath)
  magick::image_write(img, path = orig_path, format = "jpeg", quality = 85)

  crop_geom <- base::paste0(
    crop_data$width,
    "x",
    crop_data$height,
    "+",
    crop_data$x,
    "+",
    crop_data$y
  )
  img |>
    magick::image_crop(crop_geom) |>
    magick::image_resize("400x400") |>
    magick::image_write(path = thumb_path, format = "jpeg", quality = 85)

  # 4. Datenbank aktualisieren
  img_url <- base::paste0("original_images/", img_id, ".jpg")
  thumb_url <- base::paste0("thumbnail_images/", img_id, ".jpg")

  update_image_urls(pool_con, img_id, img_url, thumb_url)
  save_location_entry(pool_con, img_id, tag_id, map_center$lng, map_center$lat)
}
