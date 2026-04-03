upload_form_ui <- function(id) {
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
    )
  )
}

upload_form_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
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
    upload_data <- upload_form_server("upload1")

    output$dev_output <- shiny::renderPrint({
      shiny::req(upload_data())
      upload_data()
    })
  }

  shiny::shinyApp(ui, server)
}
