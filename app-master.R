library(shiny)
library(leaflet)
library(shinydashboard)
library(fresh)
library(sf)
library(emojifont)
library(shinyjs)

#load in the file that defines all the postcode region borders
source("create-map-regions.R")

#load in the file that contains the data frame to be displayed
source("data-processing.R")

# Define the import statement for Hammersmith One font
font_import_statement <- "https://fonts.googleapis.com/css?family=Hammersmith+One"

# Create a custom theme for the app using the fresh library
my_theme <- create_theme(
  adminlte_color(light_blue = "black"),
  adminlte_global(content_bg = "white"),
  adminlte_sidebar(width = "45%"),
  custom_css = sprintf(
    ".main-header .logo { font-family: 'Hammersmith One', sans-serif; font-weight: 400; font-size: 24px; }",
    font_import_statement
  )
)

# Create custom styles for specific text elements
title_style <- "font-family: 'Hammersmith One', sans-serif; font-weight: 400;"
header_style <- "font-family: 'Hammersmith One', sans-serif; font-size:130%; text-align: center; font-weight: 600;"
text_style <- "font-family: 'Hammersmith One', sans-serif; font-size:115%"
text_body <- "font-family: 'Hammersmith One', sans-serif; font-size:115%; padding-left:5%; padding-right:5%"

#header styling
dbHeader <- dashboardHeader(
  
  #define the title 
  title = span("TFL Bike Activity", style = title_style)
)

# Define the UI
ui <- dashboardPage(
  
  #load in the header that is defined above
  dbHeader,
  
  #Sidebar formatting and outputs
  dashboardSidebar(
    div(style = "max-height: 100vh; overflow-y: auto;",
        
      #explanation text
      div(style = "height: 10px;"),
      div("The 'Daily Activity' of a postcode is defined as the total number of journeys in or out of a bike station
        within the postcode in a single day.", style = text_body),
      
      #render the title for the selected postcode
      div(style = "height: 15px;border-bottom: 2px solid white;"),
      div(style = "height: 15px;"),
      uiOutput("PostcodeTitle"),
      
      #number of stations
      div(style = "height: 15px;"),
      fluidRow(
        column(width=1),
        column(width=7, "Number of Docking Stations", style = text_style),
        column(width=3, uiOutput("number"), style = text_style)
      ),
      
      #mean daily activity
      div(style = "height: 7px;"),
      fluidRow(
        column(width=1),
        column(width=7, "Mean Daily Activity", style = text_style),
        column(width=3, uiOutput("mean"), style = text_style)
      ),
      
      #Daily Standard Deviation
      div(style = "height: 7px;"),
      fluidRow(
        column(width=1),
        column(width=7, "Standard Deviation", style = text_style),
        column(width=3, uiOutput("sd"), style = text_style)
      ),
      
      #Daily Max
      div(style = "height: 7px;"),
      fluidRow(
        column(width=1),
        column(width=7, "Maximum Activity", style = text_style),
        column(width=3, uiOutput("max"), style = text_style)
      ),
      
      #Max ratio
      div(style = "height: 7px;"),
      fluidRow(
        column(width=1),
        column(width=7, "Max Activity per Station", style = text_style),
        column(width=3, uiOutput("ratio"), style = text_style)
      ),
      
      #Title for first plot
      div(style = "height: 25px;"),
      div("Recorded Daily Activity", style = header_style),
      
      #make a plot of the overall data for each postcode
      #overlay the smoothed data
      div(style = "height: 7px;"),
      fluidRow(
        column(width = 1),
        column(
          width = 10, div(style = "border-radius: 15px; height: 250px; overflow: hidden;",
                          plotOutput("SmoothedPlot"))
        ),
        column(width = 1)
      ),
      
      #explanation text
      div(style = "height: 30px;"),
      div("By running a stochastic simulation to emulate the variable probability of bikes
          entering and leaving a central london postcode throughout a working day, we can infer that if the
          daily activity per station exceedes roughly 280, the number of docked bikes will exceed
          the network's capacity and the service will become ineffective, resulting in a loss in revenue.
          We optimise the number of proposed new stations to minimise the loss when future demand exceeds
          an activity of 280 per station, while preventing overspending on new infrastructure.", style = text_body),
      
      #Title for simulation plot
      div(style = "height: 35px;"),
      div("Daily Capacity Simulation", style = header_style),
      
      #make a plot to present the simulation results, where docked bikes exceed capacity
      div(style = "height: 7px;"),
      fluidRow(
        column(width = 1),
        column(
          width = 10, div(style = "border-radius: 15px; height: 280px; overflow: hidden;",
                          plotOutput("SimulationPlot"))
        ),
        column(width = 1)
      ),
      
      #end section with baseline
      div(style = "height: 30px;border-bottom: 2px solid white;"),
      div(style = "height: 100px;")
    )
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
  
  #initiate javascript to show model on launch
  shinyjs::runjs('
    $(document).ready(function(){
      // Show the modal when the app starts
      $("#myModal").modal("show");
    });
  ')
  
  # Define the modal to display information about the app.
  showModal(
    modalDialog(
      title = "Welcome to the TFL Bike Hire Demand Forecast", "Transport for London (TFL) must proactively expand the santander bike docking station infrastructure.
          By analysing current demand patterns and projecting future growth in demand, we can
          advice TFL on strategic station expansion plans, to ensure that the
          demanded daily activity (the number of journeys in or out of a postcode per day)
          does not exceed the network's capacity This will help TFL remain as profitable as
          possible by maximising their daily number of riders, without overspending. Cick a postcode region
          on the map for more information.",
      footer = actionButton("modalOkBtn", "OK")
    )
  )
  
  # Add an observer to close the modal when the OK button is clicked
  observeEvent(input$modalOkBtn, {
    removeModal()
  })
  
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
    HTML(paste0("<div style='text-align: center; font-size: 30px;
                font-weight: bold;'>", paste0("Postcode: ",postcode_title()), "</div>"))
  })
  
  #observe for changes and fill in the descriptive statistics using
  #the data from a data frame created in data-processing.R
  observe({
    
    #search for the values from the data based on the input
    number <- postcode_statistics$Stations[postcode_statistics$Postcode == rv$postcode]
    mean <- postcode_statistics$Mean[postcode_statistics$Postcode == rv$postcode]
    sd <- postcode_statistics$sd[postcode_statistics$Postcode == rv$postcode]
    max <- postcode_statistics$max[postcode_statistics$Postcode == rv$postcode]
    ratio <- postcode_statistics$Ratio[postcode_statistics$Postcode == rv$postcode]
    emoji <- postcode_statistics$Emoji[postcode_statistics$Postcode == rv$postcode]
    colour <- postcode_statistics$Colour[postcode_statistics$Postcode == rv$postcode]
    
    ratio_str <- paste0(format(ratio, digits = 2), "  ", emoji(emoji))
    
    #renderText() the values to the UI elements above
    output$number <- renderText(number)
    output$mean <- renderText(mean)
    output$sd <- renderText(sd)
    output$max <- renderText(max)
    output$ratio <- renderText({
      HTML(paste0("<span style='font-weight: 600; color:", colour, "'>", ratio_str, "</span>"))
    })
    
    #find the correct data frame that is made in data-processing.R
    postcode_data <- paste0(rv$postcode,"_data")
    df <- get(postcode_data)
    
    #plot the data and smoothed data for the selected postcode
    output$SmoothedPlot <- renderPlot({
      par(mar=c(13,5,1.5,1), bg = "#636363", bty = "n")
      
      #plot the raw data in a light plot
      plot(df$Raw, type = "l", col = "#828282", xlab = "", ylab = "",
           ylim = c(0, 4000),
           xlim = c(1, length(df$Smooth)), xaxt = "n")
      
      #overlay and highlight the smoothed data
      lines(df$Smooth, col = "red")
      
      #manually define the axis labels for the available data
      values_to_show <- seq(1, length(df$Smooth), length.out = 4)
      axis(1, at = values_to_show, labels = c("Aug ’18", "Nov ’19", "Mar ’21", "Jul ’22"), col.axis = "white")  # Set white axis color
      axis(2, col.axis = "white")
      mtext(text = "Daily Activity", side = 2, line = 3, col = "white", cex = 1.5)
      
    })
    
    #plot the simulation data
    output$SimulationPlot <- renderPlot({
      par(mar=c(13,5,1.5,1), bg = "#636363", bty = "n")
      
      #plot the raw data in a light plot
      plot(sim_results$Capacity, type = "l", col = "#828282", xlab = "", ylab = "",
           ylim = c(0, 700),
           xlim = c(1, 6000))
      lines(sim_results$MaxCapacity, col = "red")
      
      #define the legend
      legend("bottomleft", legend = c("Bikes Docked in Postcode if 280 Journeys per Station per Day", "Max Capacity of Postcode"),
             col = c("#828282", "red"), lty = 1, lwd = 2, cex = 1.25)
      
      #define the axis formatting
      values_to_show <- seq(1, length(df$Smooth), length.out = 4)
      axis(1, col.axis = "white") 
      axis(2, col.axis = "white")
      mtext(text = "Postcode Capacity", side = 2, line = 3, col = "white", cex = 1.5)
      mtext(text = "Journeys Made In/Out Over 1 Day in a Single Postcode", side = 1, line = 3, col = "white", cex = 1.5)
      
    })
  })
  
}

# Launch the app
shinyApp(ui, server)
