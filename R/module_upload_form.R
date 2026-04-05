upload_form_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "upload-container",
      shiny::fileInput(
        inputId = shiny::NS(id, "image_upload"),
        label = "Bilder auswählen oder hierher ziehen",
        multiple = TRUE,
        accept = c(
          "image/jpeg",
          "image/png",
          "image/heic",
          ".heic",
          ".jpg",
          ".jpeg",
          ".png"
        )
      )
    ),
    shiny::selectizeInput(
      inputId = ns("tag_name_input"),
      label = "Tag Name (auswählen oder neu eingeben)",
      choices = NULL,
      multiple = FALSE,
      options = base::list(create = TRUE, dropdownParent = "body")
    ),
    shiny::dateInput(
      inputId = ns("date_input"),
      label = "Aufnahmedatum",
      value = Sys.Date()
    )
  )
}

upload_form_server <- function(id, pool_con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    options(shiny.maxRequestSize = 30 * 1024^2)

    shiny::observe({
      tags_df <- query_data_for_selector(pool_con)
      shiny::updateSelectizeInput(
        session = session,
        inputId = "tag_name_input",
        choices = tags_df$tag_name,
        server = TRUE
      )
    })

    extracted_data <- shiny::reactive({
      shiny::req(input$image_upload)

      uploaded_files <- input$image_upload

      exif_data <- exifr::read_exif(
        uploaded_files$datapath,
        tags = c("GPSLatitude", "GPSLongitude", "DateTimeOriginal")
      )

      processed_data <- exif_data |>
        dplyr::mutate(
          original_name = uploaded_files$name,
          temp_datapath = uploaded_files$datapath
        ) |>
        dplyr::rename(dplyr::any_of(c(
          lat = "GPSLatitude",
          lng = "GPSLongitude",
          date_created = "DateTimeOriginal"
        ))) |>
        dplyr::select(dplyr::any_of(c(
          "original_name",
          "temp_datapath",
          "lat",
          "lng",
          "date_created"
        )))

      if (!base::is.na(processed_data$date_created[1])) {
        parsed_date <- base::as.Date(
          stringr::str_sub(processed_data$date_created[1], 1, 10),
          format = "%Y:%m:%d"
        )
        shiny::updateDateInput(session, "date_input", value = parsed_date)
      }

      return(processed_data)
    })

    return(extracted_data)
  })
}

upload_form_app <- function() {
  ui <- shiny::fluidPage(
    upload_form_ui("upload1"),
    shiny::verbatimTextOutput("dev_output")
  )

  server <- function(input, output, session) {
    pool_con <- open_db_pool("inst/extdata/tom_database.sqlite")

    shiny::onStop(function() {
      pool::poolClose(pool_con)
    })

    upload_data <- upload_form_server("upload1", pool_con)

    output$dev_output <- shiny::renderPrint({
      shiny::req(upload_data())
      upload_data()
    })
  }

  shiny::shinyApp(ui, server)
}
