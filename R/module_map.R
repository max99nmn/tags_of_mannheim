#' Leaflet Map UI Module
#'
#' @description Creates the user interface (UI) for a leaflet map. This
#'   function is intended to be used as a Shiny module.
#'
#' @param id A `character` string. The namespace ID for the module.
#'
#' @return A `shiny.tag` object with the leaflet map output UI.
#'
#' @importFrom shiny NS
#' @importFrom leaflet leafletOutput
#' @export
map_ui <- function(id) {
  leaflet::leafletOutput(shiny::NS(id, "map"), height = "87vh")
}

#' Leaflet Map Server Module
#'
#' @description
#' The server-side logic for the leaflet map module. It renders the initial
#' map and reactively updates circle markers based on the provided data.
#'
#' @param id A `character` string. The namespace ID for the module.
#' @param map_data A reactive `data.frame`. Must contain the columns `lng`
#'   (longitude), `lat` (latitude), `color` (marker fill color), and `loc_id`
#'   (a unique layer ID for each marker).
#'
#' @return No return value. Called for its side effects (rendering the map).
#'
#' @importFrom shiny moduleServer observe
#' @importFrom leaflet renderLeaflet leaflet leafletProxy addProviderTiles
#'   providerTileOptions setView clearMarkers addCircleMarkers providers
map_server <- function(id, map_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #render static map
    output$map <- renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Stadia.AlidadeSmoothDark,
          options = leaflet::providerTileOptions(noWrap = TRUE)
        ) |>
        leaflet::setView(8.466772, 49.488661, zoom = 14)
    })

    #add marker dynamically
    observe({
      leaflet::leafletProxy(ns("map"), data = map_data()) |>
        leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          fillColor = ~color,
          radius = 5,
          stroke = FALSE,
          fillOpacity = 0.8,
          layerId = ~loc_id
        )
    })
  })
}

map_app <- function(map_data) {
  ui <- fluidPage(
    map_ui("map1")
  )

  server <- function(input, output, session) {
    map_data_reactive <- reactive({
      map_data
    })

    map_server("map1", map_data_reactive)
  }

  shiny::shinyApp(ui, server)
}
