upload_thumbnail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(
        href = "https://cdnjs.cloudflare.com/ajax/libs/cropperjs/1.5.13/cropper.min.css",
        rel = "stylesheet"
      ),
      shiny::tags$script(
        src = "https://cdnjs.cloudflare.com/ajax/libs/cropperjs/1.5.13/cropper.min.js"
      )
    ),
    shiny::uiOutput(ns("cropper_container")),
    shiny::actionButton(
      ns("save_crop"),
      "Zuschnitt bestätigen",
      class = "btn-primary",
      style = "margin-top: 15px; width: 100%;"
    )
  )
}

upload_thumbnail_server <- function(id, current_image_path) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cropper_container <- shiny::renderUI({
      shiny::req(current_image_path())

      b64_img <- knitr::image_uri(current_image_path())

      shiny::tagList(
        shiny::div(
          # aspect-ratio: 1 macht den Container quadratisch
          style = "width: 100%; aspect-ratio: 1; background-color: #222; overflow: hidden;",
          shiny::img(
            id = ns("image_to_crop"),
            src = b64_img,
            style = "max-width: 100%; display: block;"
          )
        ),
        shiny::tags$script(shiny::HTML(base::paste0(
          "
          setTimeout(function() {
            let image = document.getElementById('",
          ns("image_to_crop"),
          "');
            if (window.cropper) { window.cropper.destroy(); }
            window.cropper = new Cropper(image, {
              aspectRatio: 1,
              viewMode: 3,
              dragMode: 'move',
              autoCropArea: 1, 
              cropBoxMovable: false,
              cropBoxResizable: false,
              toggleDragModeOnDblclick: false
            });
            
            document.getElementById('",
          ns("save_crop"),
          "').onclick = function() {
              let cropData = window.cropper.getData(true);
              Shiny.setInputValue('",
          ns("crop_data"),
          "', cropData, {priority: 'event'});
            };
          }, 150);
        "
        )))
      )
    })

    # Gibt nun Pfad und Crop-Infos zurück
    crop_info <- shiny::eventReactive(input$crop_data, {
      shiny::req(current_image_path(), input$crop_data)

      base::list(
        image_path = current_image_path(),
        crop_data = input$crop_data
      )
    })

    return(crop_info)
  })
}

upload_thumbnail_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Test: Upload Thumbnail Modul"),
    shiny::fluidRow(
      shiny::column(width = 6, upload_thumbnail_ui("thumb1")),
      shiny::column(
        width = 6,
        shiny::h4("Ausgabe des Moduls (Pfad & Crop-Daten):"),
        shiny::verbatimTextOutput("module_output")
      )
    )
  )

  server <- function(input, output, session) {
    hardcoded_path <- base::file.path(
      base::getwd(),
      "inst/app/www/original_images/Osiris.HEIC"
    )

    path_reactive <- shiny::reactive({
      if (!base::file.exists(hardcoded_path) || hardcoded_path == "") {
        shiny::showNotification(
          "HINWEIS: Hardcoded Bildpfad wurde nicht gefunden oder nicht angepasst.",
          type = "warning",
          duration = NULL
        )
        return(NULL)
      }
      hardcoded_path
    })

    # Modul aufrufen
    crop_result <- upload_thumbnail_server("thumb1", path_reactive)

    # Ausgabe der übergebenen Liste (Pfad und Daten)
    output$module_output <- shiny::renderPrint({
      shiny::req(crop_result())
      base::print(crop_result())
    })
  }

  shiny::shinyApp(ui, server)
}
