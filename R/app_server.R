#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  pool_con <- open_db_pool("inst/extdata/tom_database2.sqlite")

  current_upload_index <- shiny::reactiveVal(1)

  shiny::onStop(function() {
    pool::poolClose(pool_con)
  })

  shared_selection <- shiny::reactiveVal(NULL)

  selector_data <- shiny::reactive({
    current_upload_index()
    query_data_for_selector(pool_con)
  })

  selected_values <- selector_server(
    "selector1",
    data_for_selector = selector_data
  )

  map_data <- shiny::reactive({
    current_upload_index()
    query_locations_for_map(pool_con, selected_values(), color_palette)
  })

  map_output <- map_server("map1", map_data, shared_selection)

  list_click <- list_server(
    "list1",
    map_data,
    map_output$map_bounds,
    shared_selection
  )

  shiny::observeEvent(list_click(), {
    shared_selection(list_click())
  })

  # Upload Tab Logic
  existing_locations <- shiny::reactive({
    current_upload_index()
    query_locations_for_map(pool_con, selector_data()$tag_id, color_palette)
  })

  disable_save_btn <- shiny::reactive({
    current_upload_index() > base::nrow(upload_inputs$files())
  })

  upload_inputs <- upload_form_server(
    id = "upload_form",
    tags_df = selector_data,
    disable_save = disable_save_btn,
    current_index = current_upload_index
  )

  current_upload_image <- shiny::reactive({
    shiny::req(upload_inputs$files())
    upload_inputs$files() |> dplyr::slice(current_upload_index())
  })

  shiny::observeEvent(upload_inputs$skip_click(), {
    current_upload_index(current_upload_index() + 1)
  })

  map_center <- upload_map_server(
    "upload_map",
    current_upload_image,
    existing_locations
  )
  crop_result <- upload_thumbnail_server(
    "upload_thumbnail",
    shiny::reactive(current_upload_image()$temp_datapath)
  )

  shiny::observeEvent(upload_inputs$files(), {
    current_upload_index(1)
  })

  shiny::observeEvent(upload_inputs$save_click(), {
    shiny::req(current_upload_image(), crop_result(), map_center())

    process_and_save_upload(
      pool_con = pool_con,
      current_img = current_upload_image(),
      crop_data = crop_result()$crop_data,
      map_center = map_center(),
      tag_name = upload_inputs$tag(),
      date_created = as.character(upload_inputs$date())
    )

    current_upload_index(current_upload_index() + 1)
  })
}
