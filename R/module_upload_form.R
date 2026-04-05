upload_form_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      class = "upload-container",
      shiny::fileInput(
        inputId = ns("image_upload"),
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
    shiny::uiOutput(ns("status_text")),
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
    ),
    shiny::div(
      style = "display: flex; gap: 10px; margin-top: 15px;",
      shiny::actionButton(
        ns("skip_upload"),
        "Überspringen",
        class = "btn-warning",
        style = "flex: 1;"
      ),
      shiny::actionButton(
        ns("save_upload"),
        "Speichern & Nächstes",
        class = "btn-success",
        style = "flex: 1;"
      )
    )
  )
}

upload_form_server <- function(id, tags_df, disable_save, current_index) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    options(shiny.maxRequestSize = 30 * 1024^2)

    output$status_text <- shiny::renderUI({
      shiny::req(extracted_files(), current_index())
      total <- base::nrow(extracted_files())
      idx <- current_index()

      if (idx <= total) {
        current_file <- extracted_files() |> dplyr::slice(idx)
        shiny::tags$p(
          style = "margin-top: 10px; margin-bottom: 10px; font-size: 14px;",
          shiny::tags$strong(base::paste0("Bild ", idx, " von ", total, ": ")),
          current_file$original_name[1]
        )
      } else {
        shiny::tags$p(
          style = "margin-top: 10px; margin-bottom: 10px; font-size: 14px;",
          "Alle Bilder bearbeitet."
        )
      }
    })

    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "tag_name_input",
        choices = tags_df()$tag_name,
        server = TRUE
      )
    })

    shiny::observe({
      if (disable_save()) {
        shinyjs::disable("save_upload")
        shinyjs::disable("skip_upload")
      } else {
        shinyjs::enable("save_upload")
        shinyjs::enable("skip_upload")
      }
    })

    extracted_files <- shiny::reactive({
      shiny::req(input$image_upload)
      uploaded_files <- input$image_upload

      exif_data <- exifr::read_exif(
        uploaded_files$datapath,
        tags = c("GPSLatitude", "GPSLongitude", "DateTimeOriginal")
      )

      exif_data |>
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
    })

    shiny::observeEvent(current_index(), {
      shiny::req(extracted_files())
      idx <- current_index()

      if (idx <= base::nrow(extracted_files())) {
        current_file <- extracted_files() |> dplyr::slice(idx)

        if (!base::is.na(current_file$date_created[1])) {
          parsed_date <- base::as.Date(
            stringr::str_sub(current_file$date_created[1], 1, 10),
            format = "%Y:%m:%d"
          )
          shiny::updateDateInput(session, "date_input", value = parsed_date)
        } else {
          shiny::updateDateInput(session, "date_input", value = Sys.Date())
        }
      }
    })

    return(base::list(
      files = extracted_files,
      tag = shiny::reactive(input$tag_name_input),
      date = shiny::reactive(input$date_input),
      save_click = shiny::reactive(input$save_upload),
      skip_click = shiny::reactive(input$skip_upload)
    ))
  })
}

upload_form_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Upload Module Test App"),
    shiny::mainPanel(
      upload_form_ui("test_upload")
    )
  )

  server <- function(input, output, session) {
    # Provide reactive dummy data required by the module
    mock_tags <- shiny::reactive({
      data.frame(
        tag_name = c("Urlaub", "Arbeit", "Familie"),
        stringsAsFactors = FALSE
      )
    })

    mock_disable <- shiny::reactiveVal(FALSE)
    mock_index <- shiny::reactiveVal(1)

    # Call the module
    module_output <- upload_form_server(
      id = "test_upload",
      tags_df = mock_tags,
      disable_save = mock_disable,
      current_index = mock_index
    )

    # Basic logic to test the buttons and increment the index
    shiny::observeEvent(module_output$save_click(), {
      shiny::showNotification("Gespeichert!")
      mock_index(mock_index() + 1)
    })

    shiny::observeEvent(module_output$skip_click(), {
      shiny::showNotification("Übersprungen!")
      mock_index(mock_index() + 1)
    })
  }

  shiny::shinyApp(ui, server)
}
