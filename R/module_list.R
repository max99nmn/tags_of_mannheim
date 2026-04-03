list_ui <- function(id) {
  shiny::uiOutput(
    shiny::NS(id, "cards_list"),
    style = "height: 87vh; overflow-y: auto; padding: 15px;"
  )
}

list_server <- function(id, map_data, map_bounds, shared_selection) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    precomputed_cards <- shiny::reactive({
      data <- map_data()
      if (base::nrow(data) == 0) {
        return(base::list())
      }

      cards <- purrr::map(1:base::nrow(data), function(i) {
        row <- data[i, ]
        shiny::div(
          id = ns(base::paste0("card_", row$loc_id)),
          class = "location-card",
          style = base::paste0(
            "border: 2px solid ",
            row$color,
            "; border-radius: 8px; padding: 10px; margin-bottom: 10px; cursor: pointer; display: flex; align-items: center; background-color: rgba(34,34,34,0.8); transition: all 0.2s;"
          ),
          onclick = base::paste0(
            "Shiny.setInputValue('",
            ns("card_click"),
            "', ",
            row$loc_id,
            ", {priority: 'event'})"
          ),
          shiny::img(
            src = base::paste0("www/", row$thumbnail_url),
            style = "width: 65px; height: 65px; object-fit: cover; border-radius: 5px; margin-right: 15px; cursor: zoom-in;",
            onclick = base::paste0(
              "event.stopPropagation(); Shiny.setInputValue('",
              ns("image_click"),
              "', '",
              row$image_url,
              "', {priority: 'event'});"
            )
          ),
          shiny::div(
            shiny::h4(
              row$tag_name,
              style = "margin: 0 0 5px 0; color: white; font-size: 16px;"
            ),
            shiny::p(
              base::paste("Date:", row$date_added),
              style = "margin: 0; font-size: 0.8em; color: #aaa;"
            )
          )
        )
      })
      base::names(cards) <- base::as.character(data$loc_id)
      cards
    })

    output$cards_list <- shiny::renderUI({
      shiny::req(map_bounds())
      data <- map_data()
      sel <- shared_selection()

      if (base::nrow(data) == 0) {
        return(NULL)
      }

      visible_ids <- data |>
        dplyr::filter(
          lat < map_bounds()$north,
          lat > map_bounds()$south,
          lng < map_bounds()$east,
          lng > map_bounds()$west
        ) |>
        dplyr::pull(loc_id) |>
        base::as.character()

      visible_cards <- precomputed_cards()[visible_ids]

      if (!base::is.null(sel) && base::as.character(sel) %in% visible_ids) {
        visible_cards[[base::as.character(
          sel
        )]] <- htmltools::tagAppendAttributes(
          visible_cards[[base::as.character(sel)]],
          style = "border-color: white; background-color: #555; box-shadow: 0 0 10px rgba(255,255,255,0.5);"
        )
      }

      shiny::tagList(
        visible_cards,
        shiny::tags$script(shiny::HTML(
          base::paste0(
            "
            setTimeout(function() {
              let el = document.getElementById('",
            ns(base::paste0("card_", sel)),
            "');
              if (el) el.scrollIntoView({behavior: 'smooth', block: 'center'});
            }, 50);
          "
          )
        ))
      )
    })

    shiny::observeEvent(input$image_click, {
      shiny::showModal(shiny::modalDialog(
        title = NULL,
        size = "m",
        easyClose = TRUE,
        footer = shiny::modalButton("Schließen"),
        shiny::img(
          src = base::paste0("www/", input$image_click),
          style = "width: 100%; height: auto;"
        )
      ))
    })

    return(shiny::reactive({
      input$card_click
    }))
  })
}

list_app <- function() {}
