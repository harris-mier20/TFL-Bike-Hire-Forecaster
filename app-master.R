library(shiny)
library(leaflet)
library(shinydashboard)
library(fresh)

# Define the import statement for Hammersmith One bold
font_import_statement <- "https://fonts.googleapis.com/css?family=Hammersmith+One"

# Create a custom theme
my_theme <- create_theme(
  adminlte_color(light_blue = "black"),
  adminlte_global(content_bg = "white"),
  adminlte_sidebar(width = "400px"),
  custom_css = sprintf(
    ".main-header .logo { font-family: 'Hammersmith One', sans-serif; font-weight: 400; font-size: 24px; }",
    font_import_statement
  )
)

# Create a custom style for the title element
title_style <- "font-family: 'Hammersmith One', sans-serif; font-weight: 400;"

#header styling
dbHeader <- dashboardHeader(
  
  #define the title 
  title = span("Bike Hire Activity", style = title_style)
  
  #include the logo
  #title = tags$img(src = "logo.png")
)

# Define the UI
ui <- dashboardPage(
  
  dbHeader,
  
  #Sidebar styling
  dashboardSidebar(
    #imageOutput("image"),
    textOutput("clickMessage")
  ),
  
  #Main body styling
  dashboardBody(
    use_theme(my_theme),
    leafletOutput("mymap", height = "100vh")
  )
)

server <- function(input, output, session) {
  
  #render the logo
  output$image <- renderImage({
    list(src = "logo.png", contentType = "image/png", width = "200px")
  })
  
  # Initialize click message for troubleshooting
  click_message <- reactiveVal("")
  
  # Define the map output using the renderLeaflet() function
  output$mymap <- renderLeaflet({
    # Create the map using leaflet() and specify the map details
    leaflet() %>%
      setView(lng = -0.130, lat = 51.5194, zoom = 12) %>%
      addProviderTiles(providers$Stadia.StamenTonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(lng = -0.130, lat = 51.5194, radius = 50, layerId = "circle1") %>%
      addCircleMarkers(lng = -0.140, lat = 51.5294, radius = 50, layerId = "circle2")
  })
  
  # Listen for when the map is clicked
  observeEvent(input$mymap_marker_click, {
    # Find what element has been clicked by using its ID
    clicked_marker <- input$mymap_marker_click$id
    
    # Logic to determine output action depending on what element is clicked
    if (clicked_marker == "circle1") {
      click_message("Circle 1 is clicked")
    } else if (clicked_marker == "circle2") {
      click_message("Circle 2 is clicked")
    }
  })
  
  # Render the troubleshooting message in the message output
  output$clickMessage <- renderText({
    click_message()
  })
}

# Launch the app
shinyApp(ui, server)
