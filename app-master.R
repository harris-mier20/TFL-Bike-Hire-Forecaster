library(shiny)
library(leaflet)
library(shinydashboard)
library(fresh)
library(sf)

#load in the file that defines all the postcode regions
source("create-map-regions.R")

# Define the import statement for Hammersmith One bold font
font_import_statement <- "https://fonts.googleapis.com/css?family=Hammersmith+One"

# Create a custom theme for the app using the fresh library
my_theme <- create_theme(
  adminlte_color(light_blue = "black"),
  adminlte_global(content_bg = "white"),
  adminlte_sidebar(width = "35%"),
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
  title = span("Bike Hire Demand Forecast", style = title_style)
)

# Define the UI
ui <- dashboardPage(
  
  #load in the header that is defined above
  dbHeader,
  
  #Sidebar formatting and outputs
  dashboardSidebar(
    textOutput("PostcodeTitle") 
  ),
  
  #Main body styling
  dashboardBody(
    
    #load in the defined themes
    use_theme(my_theme),
    
    #fill the screen with the map
    div(
      style = "position: relative;",
      leafletOutput("mymap", height = "100vh"),
      
    #overlay the logo  
    div(
        imageOutput("image"),
        style = "position: absolute; top: 20px; left: 60px;")
    )
  )
)

server <- function(input, output, session) {
  
  #render the logo in the overlay imageOutput() element
  output$image <- renderImage({
    list(src = "logo.png", contentType = "image/png", width = "350px", deleteFile=FALSE)
  })
  
  # Initialize click message for troubleshooting
  click_message <- reactiveVal("")
  
  # Define the map output using the renderLeaflet() function
  output$mymap <- renderLeaflet({
    
    # Create the map using leaflet()
    leaflet() %>%
      
      #define where the map is zoomed by default
      setView(lng = -0.110, lat = 51.5200, zoom = 14) %>%
      addProviderTiles(providers$Stadia.StamenTonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      
      #add postcode regions to the map
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.5, layerId = "wc1", options = pathOptions(clickable = TRUE)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.5, layerId = "wc2", options = pathOptions(clickable = TRUE)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.5, layerId = "ec1", options = pathOptions(clickable = TRUE)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.5, layerId = "ec2", options = pathOptions(clickable = TRUE)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.5, layerId = "ec3", options = pathOptions(clickable = TRUE)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.5, layerId = "ec4", options = pathOptions(clickable = TRUE))
  
  })
  
  #wait for a shape to be clicked
  observeEvent(input$mymap_shape_click, {
    postcode <- input$mymap_shape_click$id
    
    #logic to handle actions based on what postcode is clicked
    if (postcode == "wc1") {
      click_message("WC1")
    } else if (postcode == "wc2") {
      click_message("WC2")
    } else if (postcode == "ec1") {
      click_message("EC1")
    } else if (postcode == "ec2") {
      click_message("EC2")
    } else if (postcode == "ec3") {
      click_message("EC3")
    } else if (postcode == "ec4") {
      click_message("EC4")
    }
      
  })
  
  # Render the troubleshooting message in the message output
  output$PostcodeTitle <- renderText({
    click_message()
  })
}

# Launch the app
shinyApp(ui, server)
