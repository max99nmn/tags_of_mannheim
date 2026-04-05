upload_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "position: relative; height: 400px; margin-bottom: 20px;",
      leaflet::leafletOutput(ns("minimap"), height = "100%"),
      shiny::div(
        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; pointer-events: none; color: white; font-size: 30px; text-shadow: 0 0 5px black;",
        "+"
      )
    )
  )
}

upload_map_server <- function(id, current_upload_image, existing_locs) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$minimap <- leaflet::renderLeaflet({
      data <- current_upload_image()

      if (
        !base::is.null(data) && "lat" %in% names(data) && !is.na(data$lat[1])
      ) {
        lat_init <- base::as.numeric(data$lat[1])
        lng_init <- base::as.numeric(data$lng[1])
        zoom_lvl <- 17
      } else {
        lat_init <- 49.48876
        lng_init <- 8.466682
        zoom_lvl <- 13
      }

      locs_data <- existing_locs()

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Stadia.AlidadeSmoothDark
        ) |>
        leaflet::setView(lng = lng_init, lat = lat_init, zoom = zoom_lvl)

      if (base::nrow(locs_data) > 0) {
        label_html <- base::paste0(
          "<div style='text-align:center;'>",
          "<strong>",
          locs_data$tag_name,
          "</strong><br>",
          "<img src='www/",
          locs_data$thumbnail_url,
          "' style='width:100px;height:100px;object-fit:cover;margin-top:5px;border-radius:4px;'>",
          "</div>"
        )

        map <- map |>
          leaflet::addCircleMarkers(
            data = locs_data,
            lng = ~lng,
            lat = ~lat,
            fillColor = "#D3D3D3",
            fillOpacity = 0.4,
            stroke = FALSE,
            radius = 5,
            label = base::lapply(label_html, shiny::HTML),
            labelOptions = leaflet::labelOptions(direction = "auto")
          )
      }

      map
    })

    return(shiny::reactive({
      if (base::is.null(input$minimap_center)) {
        base::list(lat = 49.48876, lng = 8.466682)
      } else {
        base::list(
          lat = input$minimap_center$lat,
          lng = input$minimap_center$lng
        )
      }
    }))
  })
}

upload_map_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Map Module Test App"),
    shiny::mainPanel(
      upload_map_ui("test_map"),
      shiny::h4("Aktueller Kartenmittelpunkt:"),
      shiny::verbatimTextOutput("map_center_out")
    )
  )

  server <- function(input, output, session) {
    mock_current_image <- shiny::reactive({
      data.frame(
        lat = 49.48876,
        lng = 8.466682
      )
    })

    mock_existing_locs <- shiny::reactive({
      data.frame(
        lat = c(49.490, 49.485),
        lng = c(8.465, 8.470),
        tag_name = c("Ort A", "Ort B"),
        thumbnail_url = c("dummy1.jpg", "dummy2.jpg"),
        stringsAsFactors = FALSE
      )
    })

    map_center <- upload_map_server(
      id = "test_map",
      current_upload_image = mock_current_image,
      existing_locs = mock_existing_locs
    )

    output$map_center_out <- shiny::renderPrint({
      map_center()
    })
  }

  shiny::shinyApp(ui, server)
}
