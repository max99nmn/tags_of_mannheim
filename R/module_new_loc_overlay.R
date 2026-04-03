new_loc_overlay_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "position: relative; height: 400px; margin-bottom: 20px;",
      leaflet::leafletOutput(ns("minimap"), height = "100%"),
      shiny::div(
        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000; pointer-events: none; color: white; font-size: 30px; text-shadow: 0 0 5px black;",
        "⌖"
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::selectizeInput(
          inputId = ns("tag_name_input"),
          label = "Tag Name (auswählen oder neu tippen)",
          choices = NULL,
          multiple = FALSE,
          options = base::list(create = TRUE, dropdownParent = "body")
        )
      ),
      shiny::column(
        width = 6,
        shiny::dateInput(
          inputId = ns("date_input"),
          label = "Aufnahmedatum",
          value = Sys.Date()
        )
      )
    )
  )
}

new_loc_overlay_server <- function(id, upload_data, pool_con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      tags_df <- query_data_for_selector(pool_con)
      shiny::updateSelectizeInput(
        session = session,
        inputId = "tag_name_input",
        choices = tags_df$tag_name,
        server = TRUE
      )
    })

    output$minimap <- leaflet::renderLeaflet({
      shiny::req(upload_data())
      data <- upload_data()

      lat_init <- base::as.numeric(data$lat[1])
      lng_init <- base::as.numeric(data$lng[1])

      if (!base::is.na(data$date_created[1])) {
        parsed_date <- base::as.Date(
          stringr::str_sub(data$date_created[1], 1, 10),
          format = "%Y:%m:%d"
        )
        shiny::updateDateInput(session, "date_input", value = parsed_date)
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
        leaflet::setView(lng = lng_init, lat = lat_init, zoom = 17)

      if (base::nrow(existing_locs) > 0) {
        popup_html <- base::paste0(
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
            fillColor = ~color,
            fillOpacity = 0.4,
            stroke = FALSE,
            radius = 8,
            popup = popup_html
          )
      }

      map
    })

    return(shiny::reactive({
      base::list(
        tag_name = input$tag_name_input,
        date = input$date_input,
        lat = input$minimap_center$lat,
        lng = input$minimap_center$lng
      )
    }))
  })
}

new_loc_overlay_app <- function() {
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

    dummy_upload_data <- shiny::reactive({
      base::data.frame(
        lat = 49.4875,
        lng = 8.4660,
        date_created = "2023:10:15 14:30:00"
      )
    })

    overlay_data <- new_loc_overlay_server(
      "overlay1",
      dummy_upload_data,
      pool_con
    )

    output$dev_output <- shiny::renderPrint({
      overlay_data()
    })
  }

  shiny::shinyApp(ui, server)
}
