#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  pool_con <- open_db_pool("inst/extdata/tom_database.sqlite")

  shiny::onStop(function() {
    pool::poolClose(pool_con)
  })

  shared_selection <- shiny::reactiveVal(NULL)

  selector_data <- shiny::reactive({
    query_data_for_selector(pool_con)
  })

  selected_values <- selector_server(
    "selector1",
    data_for_selector = selector_data
  )

  map_data <- shiny::reactive({
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
}
