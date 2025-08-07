library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(RSQLite)
library(pool)
library(DT)
library(paletteer)
library(purrr)
library(stringr)

con <- dbPool(RSQLite::SQLite(), dbname = "graffiti.sqlite")
onStop(function() {
  poolClose(con)
})

add_jitter <- function(amt, lat, lon) {
  set.seed(as.numeric(lat) * 1e7 + as.numeric(lon) * 1e7)
  rnorm(amt, sd = 0.000008)
}

create_card_html <- function(name, long, lat, img_url) {
  sprintf(
    '<div style="width: 220px; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); font-family: sans-serif; overflow: hidden; margin: 5px auto; text-align: center; background-color: #fff;">
      <img src="%s" alt="Profilbild" style="width: 100%%; height: auto; display: block;">
      <div style="padding: 10px;">
        <h3 style="margin: 10px 0 5px 0; font-size: 1.2em; color: #333;">%s</h3>
        <p style="margin: 4px 0; color: #555; font-size: 0.85em;"><strong>Lon:</strong> %s | <strong>Lat:</strong> %s</p>
      </div>
    </div>',
    img_url,
    name,
    long,
    lat
  )
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
    ),

    fileInput("file1", "Choose a File"),
    verbatimTextOutput("file1_contents"),

    selectInput(
      "tag_select",
      "Select Tag:",
      list("Onib" = "onib", "Igor" = "igor")
    ),

    dateInput(
      inputId = "date",
      label = h4("Date"),
      value = Sys.Date()
    ),

    hr(),
    verbatimTextOutput("value"),
    verbatimTextOutput("value_class"),
    verbatimTextOutput("value_year"),

    h4("Select Location on Map"),
    leafletOutput("select_map", height = 400),

    # Output for showing selected lat/lng
    h4("Selected coords"),
    verbatimTextOutput("selected_coords")
  )
)

# --- Server Logic ---
# Contains the instructions to build and run the app.
server <- function(input, output, session) {
  output$select_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(8.466772, 49.488661, zoom = 14)
  })

  # Capture the map click and display coordinates
  output$selected_coords <- renderPrint({
    req(input$select_map_click)
    click <- input$select_map_click
    cat("Longitude:", click$lng, "\nLatitude:", click$lat)
  })

  observeEvent(input$select_map_click, {
    click <- input$select_map_click
    cat("Map clicked at:\n")
    cat("  Longitude:", click$lng, "\n")
    cat("  Latitude :", click$lat, "\n")
  })

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
        ungroup() |>
        mutate(row_id = 1:nrow(db_data))
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
        fillOpacity = 0.8,
        layerId = filtered_data()$row_id
      )
  })

  debounced_bounds <- debounce(
    reactive({
      input$map_bounds
    }),
    500
  )

  data_in_bounds <- reactive({
    req(debounced_bounds())

    bounds <- debounced_bounds()

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
        latitude = round(latitude, digits = 4),
        longitude = round(longitude, digits = 4),
        html_card = pmap_chr(
          list(tag_name, longitude, latitude, image_url),
          create_card_html
        )
      ) |>
      select(html_card)

    datatable(
      table_data,
      colnames = NULL,
      escape = FALSE,
      extensions = 'Select',
      selection = 'single',
      callback = JS(
        "table.on('select.dt', function(e, dt, type, indexes) {",
        "  if (type === 'row') {",
        "    var row_node = table.row(indexes).node();",
        "    row_node.scrollIntoView({behavior: 'smooth', block: 'center'});",
        "  }",
        "});"
      ),
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      rownames = FALSE,
      class = 'display compact'
    )
  })

  visible_locations_table_proxy = dataTableProxy('visible_locations_table')

  observeEvent(input$map_marker_click, {
    row_of_selected_marker <- input$map_marker_click$id
    selectRows(visible_locations_table_proxy, selected = row_of_selected_marker)
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
