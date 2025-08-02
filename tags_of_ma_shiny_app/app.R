library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(RSQLite)
library(pool)

tag_colors <- c(
  Onib = "#1f77b4",   # Muted Blue
  Izza = "#ff7f0e",   # Safety Orange
  Minus = "#2ca02c",  # Cooked Asparagus Green
  Orni = "#d62728",   # Brick Red
  Mutton = "#9467bd", # Muted Purple
  Igor = "#8c564b"  # Chestnut Brown
)

con <- dbPool(RSQLite::SQLite(), dbname = "graffiti.sqlite")
onStop(function() {
  poolClose(con)
})

# jitter_group <- function(df, key, sd = 0.00008) {
#   # If there's only one point, no need to jitter
#   if (nrow(df) <= 1) {
#     df$latitude <- key$latitude
#     df$longitude <- key$longitude
#     return(df)
#   }
  
#   seed <- as.integer(key$latitude * 1e7) + as.integer(key$longitude * 1e7)
#   set.seed(seed)
  
#   df |> 
#     mutate(
#       latitude = key$latitude + rnorm(n(), mean = 0, sd = sd),
#       longitude = key$longitude + rnorm(n(), mean = 0, sd = sd)
#     )
# }
add_jitter <- function(amt, lat, lon){
  set.seed(as.numeric(lat)*1e7 + as.numeric(lon)*1e7)
  rnorm(amt, sd = 0.000008)
}

ui <- navbarPage(
  "Tags of Mannheim",
  tabPanel(
    "Map",
    fillPage(
      leafletOutput("map", height = "600px"),
      absolutePanel(
        id = "controls",
        top = 470, left = 40,
        width = 250,
        uiOutput("tag_filter_ui")
     )
    )
  ),
  tabPanel(
    "Table",
    mainPanel(
      tableOutput('table')
    )
  )
)

# --- Server Logic ---
# Contains the instructions to build and run the app.
server <- function(input, output, session) {
  output$tag_filter_ui <- renderUI({
    # Query the database to get all unique, non-empty tag names.
    tag_names <- dbGetQuery(con, "SELECT DISTINCT tag_name FROM graffiti_tags WHERE tag_name IS NOT NULL AND tag_name != '' ORDER BY tag_name")

    checkboxGroupInput(
      inputId = "tags_to_display",
      label = "Display Tags:",
      choices = tag_names$tag_name,
      selected = tag_names$tag_name
    )
  })

  filtered_data <- reactive({
    req(input$tags_to_display)

    # 1. Create a placeholder string like "?,?,?,?,?,?"
    placeholders <- paste(rep("?", length(input$tags_to_display)), collapse = ",")
    
    # 2. Construct the full query string with the placeholders
    query <- paste("SELECT * FROM graffiti_tags WHERE tag_name IN (", placeholders, ")")

    # 3. Execute the query, passing the selected tags as a list of parameters.
    # This is safe from SQL injection.
    db_data <- dbGetQuery(con, query, params = as.list(input$tags_to_display))

    if (nrow(db_data) > 0) {
      db_data |> 
        group_by(latitude, longitude) |> 
        mutate(
          latitude = if(n() > 1) latitude + add_jitter(n(), latitude, longitude) else latitude,
          longitude = if(n() > 1) longitude + add_jitter(n(), latitude, longitude) else longitude) |> 
        ungroup()
    } else {
      db_data
    }
  })


  output$map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) |> 
      setView(8.466772, 49.488661, zoom = 14)
  })

  observe({
    all_tags <- dbGetQuery(con, "SELECT DISTINCT tag_name FROM graffiti_tags")$tag_name
    color_fun <- colorFactor(palette = tag_colors, domain = all_tags)

    leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste(
          "<b>Tag:</b>", tag_name, "<br>",
          "<img src='", image_url, "' width='200'>" # Assuming image_url is a column
        ),
        radius = 5,
        color = ~ color_fun(tag_name),
        stroke = FALSE,
        fillOpacity = 0.8
      )
  })

  output$table <- renderTable(filtered_data())
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
