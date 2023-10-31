library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("mymap"),
  
  # create a text output to troubleshoot app functionality
  verbatimTextOutput("clickMessage")
)

server <- function(input, output, session) {
  
  # Initialize click message for troubleshooting
  click_message <- reactiveVal("")
  
  #define the map output using the renderLeaflet function
  output$mymap <- renderLeaflet({
    
    #create the map using leaflet() and use %>% to specify the details of the app
    leaflet() %>%
      
      #define the starting location and zoom, the map should zoom on central london
      setView(lng = -0.130, lat = 51.5194, zoom = 12) %>%
      addProviderTiles(providers$Stadia.StamenTonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      
      #add circles to troubleshoot action button functionality
      #define layerIDs so they can be listed to by observeEvents
      addCircleMarkers(lng = -0.130, lat = 51.5194, radius = 50, layerId = "circle1") %>%
      addCircleMarkers(lng = -0.140, lat = 51.5294, radius = 50, layerId = "circle2")
  })
  
  #listen for when the map is clicked
  observeEvent(input$mymap_marker_click, {
    
    #find what element has been clicked by using its ID
    clicked_marker <- input$mymap_marker_click$id
    
    #logic to determine output action depending on what element is clicked
    if (clicked_marker == "circle1") {
      click_message("Circle 1 is clicked")
    } else if (clicked_marker == "circle2") {
      click_message("Circle 2 is clicked")
    }
  })
  
  #render the troubleshooting message in the message output
  output$clickMessage <- renderText({
    click_message()
  })
}

#launch the app
shinyApp(ui, server)


