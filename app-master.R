library(shiny)
library(leaflet)
library(shinydashboard)
library(fresh)
library(sf)

#load in the file that defines all the postcode region borders
source("create-map-regions.R")

# Define the import statement for Hammersmith One font
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

# Create a custom style for the title element specifically
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
    
    #spacing
    div(style = "height: 15px;"),
    
    #render the title for the selected postcode
    uiOutput("PostcodeTitle")
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
        style = "position: absolute; top: 20px; left: 60px; pointer-events: none;"
      )
    )
  ),
  # Apply inline CSS to hide the sidebar toggle button
  tags$head(
    tags$style(
      HTML(".sidebar-toggle { display: none; }")
    )
  )
)


server <- function(input, output, session) {
  
  # Initialize a reactiveValues object to store the selected postcode
  rv <- reactiveValues(postcode = "wc1")
  
  #render the logo in the overlay imageOutput() element
  output$image <- renderImage({
    list(src = "assets/logo.png", contentType = "image/png", width = "350px", deleteFile=FALSE)
  })
  
  # Initialize click message for troubleshooting
  postcode_title <- reactiveVal("")
  
  # Define the map output using the renderLeaflet() function
  output$mymap <- renderLeaflet({
    
    # Create the map using leaflet()
    leaflet() %>%
      
      #define where the map is zoomed by default
      setView(lng = -0.110, lat = 51.5200, zoom = 13) %>%
      addProviderTiles(providers$Stadia.StamenTonerLite, options = providerTileOptions(noWrap = TRUE))
  })
  
  #wait for a shape to be clicked and update the reactive val accordingly
  observeEvent(input$mymap_shape_click, {
    rv$postcode <- input$mymap_shape_click$id
    print(rv$postcode)
  })
  
  #logic to handle actions based on what postcode is selected
  observeEvent(rv$postcode, {
    
    #highlight wc1 when wc1 is selected
    if (rv$postcode == "wc1") {
      postcode_title("WC1")
      leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.8, weight = 3,
                  layerId = "wc1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec3", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec4", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1))
      
    #highlight wc2 when wc2 is selected
    } else if (rv$postcode == "wc2") {
      postcode_title("WC2")
      leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.8, weight = 3,
                  layerId = "wc2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec3", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec4", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1))
    
    #highlight ec1 when ec1 is selected  
    } else if (rv$postcode == "ec1") {
      postcode_title("EC1")
      leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.8, weight = 3,
                  layerId = "ec1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec3", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec4", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1))
    
    #highlight ec2 when ec2 is selected
    } else if (rv$postcode == "ec2") {
      postcode_title("EC2")
      leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.8, weight = 3,
                  layerId = "ec2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec3", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec4", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1))
      
    #highlight ec3 when ec3 is selected
    } else if (rv$postcode == "ec3") {
      postcode_title("EC3")
      leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.8, weight = 3,
                  layerId = "ec3", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec4", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1))
      
    #highlight ec4 when ec4 is selected
    } else if (rv$postcode == "ec4") {
      postcode_title("EC4")
      leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = wc1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = wc2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "wc2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec1_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec1", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec2_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec2", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec3_sf, fillColor = "red", fillOpacity = 0.5, weight = 3,
                  layerId = "ec3", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1)) %>%
      addPolygons(data = ec4_sf, fillColor = "red", fillOpacity = 0.8, weight = 3,
                  layerId = "ec4", options = pathOptions(clickable = TRUE),
                  highlightOptions = highlightOptions(fillOpacity = 1))
    }
  }) 
  
  # Render the postcode title that output is in the sidebar
  output$PostcodeTitle <- renderUI({
    HTML(paste0("<div style='text-align: center; font-size: 24px;
                font-weight: bold;'>", postcode_title(), "</div>"))
  })
}

# Launch the app
shinyApp(ui, server)
