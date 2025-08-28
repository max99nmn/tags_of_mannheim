#' Tag Selector UI Module
#'
#' @description
#' Creates a UI component with a `shinyWidgets::virtualSelectInput` for
#' selecting up to 10 tags.
#'
#' @param id A `character` string. The namespace ID for the module.
#'
#' @note The choices for this input are initialized as `NULL` and must be
#'   populated from the server module using
#'   `shinyWidgets::updateVirtualSelectInput()`.
#'
#' @return A `shiny.tag` object representing the selector UI.
#'
#' @importFrom shiny NS
#' @importFrom shinyWidgets virtualSelectInput
#' @export
selector_ui <- function(id) {
  shinyWidgets::virtualSelectInput(
    inputId = shiny::NS(id, "virtual_select_tag_selection"),
    label = "Wähle bis zu 10 Tags:",
    choices = NULL,
    multiple = TRUE,
    showValueAsTags = TRUE,
    options = list(maxValues = 10)
  )
}

#' Tag Selector Server Module
#'
#' @description
#' The server-side logic for the tag selector module. It populates the choices
#' in the selector based on reactive data and returns the user's selection.
#'
#' @param id A `character` string. The namespace ID for the module.
#' @param data_for_selector A reactive `data.frame`. It must contain the columns
#'   `tag_id` (the values) and `tag_name` (the labels displayed to the user).
#'
#' @return A `reactive` expression that returns a character vector of the
#'   selected tag IDs.
#'
#' @importFrom shiny moduleServer observe reactive
#' @importFrom shinyWidgets updateVirtualSelect
#' @export
selector_server <- function(id, data_for_selector) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      selector_values <- data_for_selector()$tag_id
      base::names(selector_values) <- data_for_selector()$tag_name
      selector_values <- base::as.list(selector_values)

      shinyWidgets::updateVirtualSelect(
        inputId = "virtual_select_tag_selection",
        session = session,
        choices = selector_values,
        selected = selector_values
      )
    })

    return(shiny::reactive({
      input$virtual_select_tag_selection
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
