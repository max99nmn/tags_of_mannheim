#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      padding = 0,
      margin = 0,
      theme = bslib::bs_theme(bootswatch = "darkly"),

      navbarPage(
        "Tags of Mannheim",
        id = "nav",

        tabPanel(
          "Map",
          div(
            class = "main-content-wrapper",

            # Left side: Map and Selector modules
            div(
              class = "map-container",
              style = "position: relative;",

              div(
                style = "position: absolute; top: 10px; right: 10px; z-index: 1000; width: 350px; background-color: rgba(34, 34, 34, 0.85); padding: 15px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);",
                selector_ui("selector1")
              ),

              map_ui("map1")
            ),

            # Right side: Details sidebar module
            #card_list_ui("card_list1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ToMA"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
