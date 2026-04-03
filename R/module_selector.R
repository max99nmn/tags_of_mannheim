#' Tag Selector UI Module (Selectize Version)
#'
#' @description
#' Creates a UI component using `shiny::selectizeInput` for selecting multiple
#' tags.
#'
#' @param id A `character` string. The namespace ID for the module.
#'
#' @return A `shiny.tag` object with the selectize input UI.
#'
#' @note The choices for this input are initialized as `NULL` and must be populated
#' from the server using `shiny::updateSelectizeInput()`. The function also
#' depends on a globally defined variable, `maximum_selector_items`, to set
#' the maximum number of selectable items.
#'
#' @importFrom shiny NS selectizeInput
#' @export
selector_ui <- function(id) {
  shiny::selectizeInput(
    inputId = shiny::NS(id, "tag_selector"),
    label = "Wähle bis zu 5 Tags:",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = maximum_selector_items)
  )
}

#' Tag Selector Server Module (Selectize Version)
#'
#' @description
#' The server-side logic for the `selectizeInput` tag selector. This function
#' populates the selector's choices, pre-selects a random number of items
#' up to a defined maximum, and returns the user's current selection.
#'
#' @param id A `character` string. The namespace ID for the module.
#' @param data_for_selector A reactive `data.frame`. Must contain `tag_id`
#'   (the values) and `tag_name` (the labels) columns.
#'
#' @return A `reactive` expression containing a character vector of the
#'   selected tag IDs.
#'
#' @note This function depends on a globally defined variable,
#'   `maximum_selector_items`, to control the number of randomly pre-selected
#'   tags. It also uses `server = TRUE` for better performance with large
#'   choice sets.
#'
#' @importFrom shiny moduleServer observe reactive updateSelectizeInput
#' @importFrom stats runif
#' @export
selector_server <- function(id, data_for_selector) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observe({
      selector_values <- data_for_selector()$tag_id
      base::names(selector_values) <- data_for_selector()$tag_name
      selector_values <- base::as.list(selector_values)

      shiny::updateSelectizeInput(
        session = session,
        inputId = "tag_selector",
        choices = selector_values,
        selected = selector_values[stats::runif(
          maximum_selector_items,
          1,
          base::length(selector_values)
        )],
        options = base::list(
          maxItems = maximum_selector_items
        ),
        server = TRUE
      )
    })

    return(shiny::reactive({
      input$tag_selector
    }))
  })
}

selector_app <- function(test_data_static) {
  ui <- shiny::fluidPage(
    selector_ui("selector"),
    shiny::verbatimTextOutput("dev_output")
  )

  server <- function(input, output, session) {
    test_data_reactive <- shiny::reactive({
      test_data_static
    })

    selected_values <- selector_server(
      "selector",
      data_for_selector = test_data_reactive
    )

    output$dev_output <- shiny::renderPrint({
      paste("selected tag_ids:", paste(selected_values(), collapse = ", "))
    })
  }

  shiny::shinyApp(ui, server)
}
