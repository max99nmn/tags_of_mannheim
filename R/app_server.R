#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #building db connection
  pool_con <- open_db_pool("inst/extdata/tom_database.sqlite")

  #ensure that pool is closed properly when closing the app
  shiny::onStop(function() {
    pool::poolClose(pool_con)
  })

  #selector logic
  selector_data <- shiny::reactive({
    query_data_for_selector(pool_con)
  })

  selected_values <- selector_server(
    "selector1",
    data_for_selector = selector_data
  )

  #map logic
  map_data <- shiny::reactive({
    req(selected_values())
    query_locations_for_map(pool_con, selected_values(), color_palette)
  })

  map_output <- map_server("map1", map_data)
}
