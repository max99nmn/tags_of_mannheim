library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(RSQLite)
library(pool)
library(DT)
library(paletteer)

con <- dbPool(RSQLite::SQLite(), dbname = "graffiti.sqlite")
onStop(function() {
  poolClose(con)
})

add_jitter <- function(amt, lat, lon) {
  set.seed(as.numeric(lat) * 1e7 + as.numeric(lon) * 1e7)
  rnorm(amt, sd = 0.000008)
}

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  "Tags of Mannheim",
  tabPanel(
    "Map",
    splitLayout(
      cellWidths = c("70%", "30%"),
      leafletOutput("map", height = "87vh"),
      div(
        style = "height: 87vh; overflow-y: auto;",
        DTOutput("visible_locations_table")
      )
    ),
    absolutePanel(
      id = "controls",
      bottom = 20,
      left = 30,
      width = 200,
      uiOutput("tag_filter_ui")
    )
  ),
  tabPanel(
    "Add New Tag",
    h3("Add New Graffiti Tag"),
    p(
      "This section can be used to build a form for adding new entries to the database."
    )
  )
)

# --- Server Logic ---
# Contains the instructions to build and run the app.
server <- function(input, output, session) {
  output$tag_filter_ui <- renderUI({
    colors_for_tags <- tag_color_map()
    tag_names <- names(colors_for_tags)

    custom_css <- lapply(tag_names, function(tag) {
      sprintf(
        "input[type='checkbox'][value='%s'] + span { color: %s !important; font-weight: bold; }",
        tag,
        colors_for_tags[tag]
      )
    })

    tagList(
      tags$style(HTML(paste(unlist(custom_css), collapse = "\n"))),
      checkboxGroupInput(
        "tags_to_display",
        "Display Tags:",
        choices = tag_names,
        selected = tag_names
      )
    )
  })

  tag_color_map <- reactive({
    all_tags <- dbGetQuery(
      con,
      "SELECT DISTINCT tag_name FROM graffiti_tags WHERE tag_name IS NOT NULL AND tag_name != '' ORDER BY tag_name"
    )$tag_name

    # length.out to repeat colors if there are more tags than palette colors
    palette <- rep(
      paletteer_d("ggthemes::Classic_10"),
      length.out = length(all_tags)
    )

    setNames(palette, all_tags)
  })

  filtered_data <- reactive({
    req(input$tags_to_display)

    # 1. Create a placeholder string like "?,?,?,?,?,?"
    placeholders <- paste(
      rep("?", length(input$tags_to_display)),
      collapse = ","
    )

    # 2. Construct the full query string with the placeholders
    query <- paste(
      "SELECT * FROM graffiti_tags WHERE tag_name IN (",
      placeholders,
      ")"
    )

    # 3. Execute the query, passing the selected tags as a list of parameters.
    # This is safe from SQL injection.
    db_data <- dbGetQuery(con, query, params = as.list(input$tags_to_display))

    if (nrow(db_data) > 0) {
      db_data |>
        group_by(latitude, longitude) |>
        mutate(
          latitude = if (n() > 1) {
            latitude + add_jitter(n(), latitude, longitude)
          } else {
            latitude
          },
          longitude = if (n() > 1) {
            longitude + add_jitter(n(), latitude, longitude)
          } else {
            longitude
          }
        ) |>
        ungroup()
    } else {
      db_data
    }
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(
        providers$Stadia.AlidadeSmoothDark,
        options = providerTileOptions(noWrap = TRUE)
      ) |>
      setView(8.466772, 49.488661, zoom = 14)
  })

  observe({
    colors_for_tags <- tag_color_map()

    color_fun <- colorFactor(
      palette = colors_for_tags,
      domain = names(colors_for_tags)
    )

    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ paste(
          "<b>Tag:</b>",
          tag_name,
          "<br>",
          "<img src='",
          image_url,
          "' width='200'>"
        ),
        radius = 5,
        fillColor = ~ color_fun(tag_name),
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })

  data_in_bounds <- reactive({
    req(input$map_bounds)

    bounds <- input$map_bounds

    filtered_data() %>%
      filter(
        latitude < bounds$north,
        latitude > bounds$south,
        longitude < bounds$east,
        longitude > bounds$west
      )
  })

  output$visible_locations_table <- renderDT({
    table_data <- data_in_bounds() |>
      mutate(
        image_preview = paste0("<img src='", image_url, "' width='150'>")
      ) |>
      select(Image = image_preview, Tag = tag_name)

    datatable(
      table_data,
      escape = FALSE,
      options = list(
        paging = FALSE,
        pageLength = 200,
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE
      ),
      rownames = FALSE,
      class = 'display compact'
    )
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
