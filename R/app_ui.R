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
          shiny::div(
            class = "main-content-wrapper",
            style = "display: flex; height: 87vh; width: 100%;",

            shiny::div(
              class = "map-container",
              style = "flex-grow: 1; position: relative;",

              shiny::div(
                style = "position: absolute; top: 10px; right: 10px; z-index: 1000; width: 350px; background-color: rgba(34, 34, 34, 0.85); padding: 15px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);",
                selector_ui("selector1")
              ),

              map_ui("map1")
            ),

            shiny::div(
              style = "width: 350px; background-color: #222;",
              list_ui("list1")
            )
          )
        ),
        tabPanel(
          "Upload",
          shiny::div(
            class = "main-content-wrapper",
            style = "display: flex; justify-content: center; align-items: center; height: 87vh; width: 100%; background-color: #111;",

            shiny::div(
              class = "module-band",
              style = "display: flex; flex-direction: row; height: 45vh; gap: 20px;",

              shiny::div(
                class = "form-container",
                style = "height: 100%; aspect-ratio: 1 / 1; background-color: #222; padding: 15px; border-radius: 8px;",
                upload_form_ui("upload_form")
              ),

              shiny::div(
                class = "map-container",
                style = "height: 100%; aspect-ratio: 1 / 1; position: relative; border-radius: 8px; overflow: hidden;",
                upload_map_ui("upload_map")
              ),

              shiny::div(
                class = "thumbnail-container",
                style = "height: 100%; aspect-ratio: 1 / 1; background-color: #222; border-radius: 8px; overflow: hidden;",
                upload_thumbnail_ui("upload_thumbnail")
              )
            )
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
