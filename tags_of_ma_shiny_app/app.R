# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)

# --- Initial Data ---
# This data frame simulates our starting database.
initial_data <- tibble(
  latitude = c(49.4875, 49.4890, 49.4830),
  longitude = c(8.4660, 8.4700, 8.4750),
  artist = c("Artist A", "Artist B", "Artist C"),
  image_url = c(
    "https://placehold.co/400x300/EBF5FF/7F9CF5?text=Graffiti+1",
    "https://placehold.co/400x300/EBF5FF/7F9CF5?text=Graffiti+2",
    "https://placehold.co/400x300/EBF5FF/7F9CF5?text=Graffiti+3"
  )
)

# --- User Interface (UI) ---
# Defines the layout of the web page.
ui <- navbarPage(
  "Tags of Mannheim",
  tabPanel(
    "Map",
    sidebarPanel(
      h4("Map Controls"),
      checkboxGroupInput(
        "tags_to_display", 
        label = "Tag Selection", 
        choices = list("Onib" = 1, "Izza" = 2, "Minus" = 3, "Orni" = 4, "Mutton?!" = 5, "Igor" = 6),
        selected = 1
      )
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# --- Server Logic ---
# Contains the instructions to build and run the app.
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) |> 
      setView(8.466772, 49.488661, zoom = 14)
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
