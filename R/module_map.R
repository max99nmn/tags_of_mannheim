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

    #get selected marker id
    selected_marker <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$map_marker_click, {
      selected_marker(input$map_marker_click$id)
    })

    #add stroke styling to map data
    styled_map_data <- shiny::reactive({
      current_selection <- selected_marker()

      if (base::is.null(current_selection)) {
        stroke_weight <- 0
        stroke_color <- "black"
        opacity <- 0.8
        radius <- 5
      } else {
        is_selected <- map_data()$loc_id == current_selection
        stroke_weight <- base::ifelse(is_selected, 5, 0)
        stroke_color <- base::ifelse(is_selected, "white", "black")
        opacity <- base::ifelse(is_selected, 1, 0.8)
        radius <- base::ifelse(is_selected, 7, 5)
      }

      map_data() |>
        dplyr::mutate(
          stroke_weight = stroke_weight,
          stroke_color = stroke_color,
          opacity = opacity,
          radius = radius
        )
    })

    #render static map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Stadia.AlidadeSmoothDark,
          options = leaflet::providerTileOptions(noWrap = TRUE)
        ) |>
        leaflet::setView(8.466772, 49.488661, zoom = 14)
    })

    #add marker dynamically
    observe({
      shiny::req(input$map_bounds)

      map_data_current <- styled_map_data()

      map_proxy <- leaflet::leafletProxy(ns("map"), data = map_data_current) |>
        leaflet::clearMarkers()

      if (base::nrow(map_data_current) > 0) {
        map_proxy |>
        leaflet::addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          fillColor = ~color,
          radius = ~radius,
          stroke = TRUE,
          weight = ~stroke_weight,
          color = ~stroke_color,
          fillOpacity = ~opacity,
          layerId = ~loc_id,
          label = ~tag_name
        )
      }
    })

    return(base::list(
      marker_click = shiny::reactive({
        input$map_marker_click
      }),
      map_bounds = shiny::reactive({
        input$map_bounds
      }),
      map_center = shiny::reactive({
        input$map_center
      })
    ))
  })
}

map_app <- function(map_data) {
  ui <- shiny::fluidPage(
    map_ui("map1"),
    shiny::verbatimTextOutput("dev_output")
  )

  server <- function(input, output, session) {
    map_data_reactive <- shiny::reactive({
      map_data
    })

    map_output <- map_server("map1", map_data_reactive)

    output$dev_output <- shiny::renderText({
      shiny::req(map_output)
      map_output_marker_click <- map_output$marker_click()
      map_output_map_bounds <- map_output$map_bounds()
      map_output_map_center <- map_output$map_center()

      base::paste0(
        "INFO OF CLICKED MARKER:\n",
        "id: ",
        map_output_marker_click$id,
        " | lat: ",
        map_output_marker_click$lat,
        " | lng: ",
        map_output_marker_click$lng,
        "\nMAP BOUNDS:\n",
        "north: ",
        map_output_map_bounds$north,
        " | east: ",
        map_output_map_bounds$east,
        " | south: ",
        map_output_map_bounds$south,
        " | west: ",
        map_output_map_bounds$west,
        "\nMAP CENTER:\n",
        "lat: ",
        map_output_map_center$lat,
        " | lng: ",
        map_output_map_center$lng
      )
    })
  }

  shiny::shinyApp(ui, server)
}
