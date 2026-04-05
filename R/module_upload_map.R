new_loc_overlay_ui <- function(id) {
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

new_loc_overlay_server <- function(id, current_upload_image, pool_con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$minimap <- leaflet::renderLeaflet({
      shiny::req(current_upload_image())
      data <- current_upload_image()

      if ("lat" %in% names(data) && !is.na(data$lat[1])) {
        lat_init <- base::as.numeric(data$lat[1])
        lng_init <- base::as.numeric(data$lng[1])
        zoom_lvl <- 17
      } else {
        lat_init <- 49.48876
        lng_init <- 8.466682
        zoom_lvl <- 13
      }

      all_tags <- query_data_for_selector(pool_con)$tag_id
      existing_locs <- query_locations_for_map(
        pool_con,
        all_tags,
        color_palette
      )

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Stadia.AlidadeSmoothDark
        ) |>
        leaflet::setView(lng = lng_init, lat = lat_init, zoom = zoom_lvl)

      if (base::nrow(existing_locs) > 0) {
        label_html <- base::paste0(
          "<div style='text-align:center;'>",
          "<strong>",
          existing_locs$tag_name,
          "</strong><br>",
          "<img src='www/",
          existing_locs$thumbnail_url,
          "' style='width:100px;height:100px;object-fit:cover;margin-top:5px;border-radius:4px;'>",
          "</div>"
        )

        map <- map |>
          leaflet::addCircleMarkers(
            data = existing_locs,
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
      base::list(
        lat = input$minimap_center$lat,
        lng = input$minimap_center$lng
      )
    }))
  })
}

new_loc_overlay_app <- function() {
  shiny::addResourcePath("www", base::file.path(base::getwd(), "inst/app/www"))

  ui <- shiny::fluidPage(
    shiny::h3("Test: Bearbeitungs-UI & Mini-Map"),
    new_loc_overlay_ui("overlay1"),
    shiny::verbatimTextOutput("dev_output")
  )

  server <- function(input, output, session) {
    pool_con <- open_db_pool("inst/extdata/tom_database.sqlite")

    shiny::onStop(function() {
      pool::poolClose(pool_con)
    })

    dummy_current_upload_image <- shiny::reactive({
      base::data.frame(
        date_created = "xyz"
      )
    })

    overlay_data <- new_loc_overlay_server(
      "overlay1",
      dummy_current_upload_image,
      pool_con
    )

    output$dev_output <- shiny::renderPrint({
      overlay_data()
    })
  }

  shiny::shinyApp(ui, server)
}
