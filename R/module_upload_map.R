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

    # Initialisierung der Karte (nur einmal)
    output$minimap <- leaflet::renderLeaflet({
      initial_locs <- shiny::isolate(existing_locs())

      upload_map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Stadia.AlidadeSmoothDark
        ) |>
        leaflet::setView(lng = 8.466682, lat = 49.48876, zoom = 13)

      if (base::nrow(initial_locs) > 0) {
        label_html <- base::paste0(
          "<div style='text-align:center;'>",
          "<strong>",
          initial_locs$tag_name,
          "</strong><br>",
          "<img src='www/",
          initial_locs$thumbnail_url,
          "' style='width:100px;height:100px;object-fit:cover;margin-top:5px;border-radius:4px;'>",
          "</div>"
        )

        upload_map |>
          leaflet::addCircleMarkers(
            data = initial_locs,
            lng = ~lng,
            lat = ~lat,
            fillColor = "#D3D3D3",
            fillOpacity = 0.4,
            stroke = FALSE,
            radius = 5,
            label = base::lapply(label_html, shiny::HTML)
          )
      }
    })

    # Update der Marker, wenn sich die DB ändert
    shiny::observe({
      locs_data <- existing_locs()

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

      proxy <- leaflet::leafletProxy("minimap", session)

      proxy |>
        leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(
          data = locs_data,
          lng = ~lng,
          lat = ~lat,
          fillColor = "#D3D3D3",
          fillOpacity = 0.4,
          stroke = FALSE,
          radius = 5,
          label = base::lapply(label_html, shiny::HTML)
        )
    })

    # Update der Kartenmitte, wenn ein neues Bild geladen wird (EXIF Daten)
    shiny::observe({
      data <- current_upload_image()
      shiny::req(data)

      if ("lat" %in% names(data) && !is.na(data$lat[1])) {
        leaflet::leafletProxy("minimap", session) |>
          leaflet::setView(
            lng = base::as.numeric(data$lng[1]),
            lat = base::as.numeric(data$lat[1]),
            zoom = 17
          )
      }
    })

    return(shiny::reactive({
      base::list(
        lat = input$minimap_center$lat,
        lng = input$minimap_center$lng
      )
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
