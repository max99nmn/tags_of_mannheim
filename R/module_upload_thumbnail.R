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
    shiny::uiOutput(ns("cropper_container"))
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
            
            image.addEventListener('crop', function(event) {
              let cropData = window.cropper.getData(true);
              Shiny.setInputValue('",
          ns("crop_data"),
          "', cropData, {priority: 'event'});
            });
          }, 150);
        "
        )))
      )
    })

    crop_info <- shiny::reactive({
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
    shiny::titlePanel("Thumbnail Modul Test App"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        upload_thumbnail_ui("test_thumb")
      ),
      shiny::mainPanel(
        shiny::h4("Aktuelle Cropping-Daten:"),
        shiny::verbatimTextOutput("crop_out")
      )
    )
  )

  server <- function(input, output, session) {
    # Nutzt ein standardmäßig in R vorhandenes Bild als Dummy
    mock_image <- shiny::reactiveVal(base::file.path(
      R.home("doc"),
      "html",
      "logo.jpg"
    ))

    crop_info <- upload_thumbnail_server(
      id = "test_thumb",
      current_image_path = mock_image
    )

    output$crop_out <- shiny::renderPrint({
      crop_info()
    })
  }

  shiny::shinyApp(ui, server)
}
