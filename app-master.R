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
title_style <- "font-family: 'Hammersmith One', sans-serif; font-weight: 400; font-size:160%"
header_style <- "font-family: 'Hammersmith One', sans-serif; font-size:130%; text-align: center; font-weight: 600;"
plot_style <- "font-family: 'Hammersmith One', sans-serif; font-size:115%; text-align: center; font-weight: 600;"
text_style <- "font-family: 'Hammersmith One', sans-serif; font-size:100%"
bold_text_style <- "font-family: 'Hammersmith One', sans-serif; font-size:100%; font-weight: 600;"
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
  
  #Sidebar formatting and outputs this is where all the data will be presented
  dashboardSidebar(
    useShinyjs(),
    
    #store everything in one master div
    div(style = "max-height: 100vh; overflow-y: auto;",
      
      #the first div describes what daily activity is and gives the postcode title
      div(
        #render the title for the selected postcode
        div(style = "height: 10px;"),
        uiOutput("PostcodeTitle"),
        
        #explain what the daily activity is
        div(style = "height: 15px;"),
        div("The 'Daily Activity' or 'Daily Demand' of a postcode is defined as the total number of journeys in or out of a bike station
          within the postcode in a single day.", style = text_body),
        
        #tick list to select what info to show.
        div(checkboxGroupInput("selectVars", "Select what information to show",
                           choices = c("Analysis of Previous Demand",
                                       "Daily Capacity Simulation",
                                       "Short Term Activity Forecast",
                                       "Long Term Activity Forecast",
                                       "Proposed Network Construction Strategy"),
                           selected = c("Descriptive Analytics")), style = "padding-left: 3%;"),
        
        #close off section with baseline
        div(style = "height: 15px;border-bottom: 2px solid white;")
      ),
      
      #this div presents descriptive information about each postcode
      div(
        
        #Title for descriptive analytics 
        div(style = "height: 20px;"),
        div("Analysis of Previous Demand", style = header_style),
        
        #number of stations
        div(style = "height: 25px;"),
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
          column(width=7, "Smoothed Maximum Activity", style = text_style),
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
        div("Recorded Daily Activity", style = plot_style),
        
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
        #close off section with baseline
        div(style = "height: 30px;border-bottom: 2px solid white;"),
        
        id = "div1", style = "display:none;"),
      
      #this div is for the simulation
      div(

        #Title for simulation
        div(style = "height: 20px;"),
        div("Daily Capacity Simulation", style = header_style),
        
        #explanation text
        div(style = "height: 25px;"),
        div("Below is a stochastic simulation to emulate the variable probability
            of bike entering and leaving a central london postcode throughout 
            a working day. Here you are able to adjust the daily 
            activity per station in the postcode to determine when docking
            demand will exceed infrastructure capacity.", style = text_body),
        
        #Title for simulation plot
        div(style = "height: 15PX;"),
        div("Simulation Results", style = plot_style),
        
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
        
        #slider to select daily activity
        div(style = "height: 25px;"),
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
        sliderInput("ActivitySlider", "Select Daily Activity", min = 65, max = 165, value = 115, width = "100%"),
        
        #end section with baseline
        div(style = "height: 30px;border-bottom: 2px solid white;"),
      id = "div2", style = "display:none;"),
      
      #this div is for short term forecasting
      div(
        
        #Title for forcasting 
        div(style = "height: 20px;"),
        div("Short Term Activity Forecast", style = header_style),
        
        #explanation of forecasting
        div(style = "height: 30px;"),
        div("This is a short term forecast that uses weather data and activity up to a given day. We are able to provide accurate 
            predictions the activity on a following day with less than 4% error across all postcodes. The following are the most important
            features and their coefficients.
            ", style = text_body),
        
        #model features
        div(style = "height: 25px;"),
        fluidRow(
          column(width=1),
          column(width=7, "Demand on Previous Day", style = text_style),
          column(width=3, uiOutput("1day"), style = text_style)
        ),
        div(style = "height: 15px;"),
        fluidRow(
          column(width=1),
          column(width=7, "Average Temperature on day (degC)", style = text_style),
          column(width=3, uiOutput("temp"), style = text_style)
        ),
        div(style = "height: 15px;"),
        fluidRow(
          column(width=1),
          column(width=7, "Wind on day (mph)", style = text_style),
          column(width=3, uiOutput("wind"), style = text_style)
        ),
        div(style = "height: 15px;"),
        fluidRow(
          column(width=1),
          column(width=7, "Rainfall on day (mm)", style = text_style),
          column(width=3, uiOutput("rain"), style = text_style)
        ),
        div(style = "height: 25px;"),
        fluidRow(
          column(width=1),
          column(width=7, "RMSE on Test Data", style = bold_text_style),
          column(width=3, uiOutput("rmse"), style = bold_text_style)
        ),
        
        #Title for short forecast plot
        div(style = "height: 20px;"),
        div("Next Day Forecast", style = plot_style),
        
        #make a plot to present the simulation results, where docked bikes exceed capacity
        div(style = "height: 7px;"),
        fluidRow(
          column(width = 1),
          column(
            width = 10, div(style = "border-radius: 15px; height: 280px; overflow: hidden;",
                            plotOutput("Shortforecastplot"))
          ),
          column(width = 1)
        ),
        
        #close off section with baseline
        div(style = "height: 30px;border-bottom: 2px solid white;"),
      id = "div3", style = "display:none;"),
      
      #this div is for long term holt winter forecasting
      div(
        
        #Title for long terms forecasting 
        div(style = "height: 20px;"),
        div("Long Term Activity Forecast", style = header_style),
        
        #explanation of long term forecasting
        div(style = "height: 30px;"),
        div("In order to make stable predictions further into the future, we use a Holt-Winters model
            that follows seasonal trends in the data.", style = text_body),
        
        #Title for long forecast plot
        div(style = "height: 20px;"),
        div("Long Term Forecast", style = plot_style),
        
        #make a plot to present the simulation results, where docked bikes exceed capacity
        div(style = "height: 7px;"),
        fluidRow(
          column(width = 1),
          column(
            width = 10, div(style = "border-radius: 15px; height: 280px; overflow: hidden;",
                            plotOutput("Longforecastplot"))
          ),
          column(width = 1)
        ),
        
        #close off section with baseline
        div(style = "height: 30px;border-bottom: 2px solid white;"),
      id = "div4", style = "display:none;"),
      
      #this div is for prescriptive analytics
      div(
        
        #Title for prescriptive analytics 
        div(style = "height: 20px;"),
        div("Proposed Network Construction Strategy", style = header_style),
        
        #explanation of long term forecasting
        div(style = "height: 30px;"),
        div("We have defined a loss function to optimise the number of stations in each postcode, minimising lost revenue for TFL.
            We acount for loss of £1.65 for each demanded journey that exceeds the capacity of the system, assuming the capaciy of the postcode is 
            the maximum daily activity per station, defined in the simulation above, multiplied by the number of stations.", style = text_body),
        div(style = "height: 10px;"),
        div("On the other hand, when capacity exceeds demand by the maximum daily activity per station, the £197,000 to build the additional station is wasted and can
            be considered lost revenue.", style = text_body),
        div(style = "height: 10px;"),
        div("Adjust the maximum daily activity per station to determine how many new stations should be built in each postcode for 2024. The simulation above
            identifies an activity of 150 to be a suitable maximum.", style = text_body),
        
        #slider to select daily activity
        div(style = "height: 25px;"),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
        sliderInput("ActivitySlider2", "Select Daily Activity", min = 65, max = 165, value = 115, width = "100%"),
        
        #Title for Bar plot
        div(style = "height: 25px;"),
        div("How Many Stations Should be Built in Each Postcode", style = plot_style),
        
        div(style = "height: 5px;"),
        # bar plot of need new stations
        fluidRow(
          column(width = 1),
          column(
            width = 10, div(style = "border-radius: 15px; height: 280px; overflow: hidden;",
                            plotOutput("NewStations"))
          ),
          column(width = 1)
        ),
        
        #add section with slider to adjust the number of stations, returning the loss in pounds over 2024
        #slider to select daily activity
        div(style = "height: 25px;"),
        fluidRow(
          column(width = 7, 
                 tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
                 sliderInput("nstations", "Select Number of New Stations", min = 0, max = 15, value = 3, width = "100%")),
          column(
            width = 3, "Test")
          ),
        
        
        #close off section with baseline
        div(style = "height: 30px;border-bottom: 2px solid white;"),
      id = "div5", style = "display:none;"),
      
      div(style = "height: 200px;")
  )),
  
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


#server processing for the app
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
      title = "How Many New Docking Stations are Needed in Central London for 2024?", "TFL must proactively expand the Santander bike docking
      station infrastructure in central London. To maximise profit, TFL must minimise the lost revenue from not meeting the growth in
      future demand, while not over spending on unecessary infrastructure. Cick postcode regions on the map to view specific details and forecasts and
      an interactive recommendation of how many stations should be built",
      footer = actionButton("modalOkBtn", "OK")
    )
  )
  
  # Add an observer to close the modal when the OK button is clicked
  observeEvent(input$modalOkBtn, {
    removeModal()
  })
  
  #hide and show divs when user selects what information to see
  observe({
    shinyjs::toggle(id = "div1", condition = "Analysis of Previous Demand" %in% input$selectVars)
    shinyjs::toggle(id = "div2", condition = "Daily Capacity Simulation" %in% input$selectVars)
    shinyjs::toggle(id = "div3", condition = "Short Term Activity Forecast" %in% input$selectVars)
    shinyjs::toggle(id = "div4", condition = "Long Term Activity Forecast" %in% input$selectVars)
    shinyjs::toggle(id = "div5", condition = "Proposed Network Construction Strategy" %in% input$selectVars)
  })
  
  # Initialize a reactiveValues object to store the selected postcode
  rv <- reactiveValues(postcode = "wc1")
  
  # Initialize a reactiveValues object to store the selected daily activity from the slider
  selected_activity <- reactive({
    input$ActivitySlider
  })
  
  # Initialize a reactiveValues object to store the selected daily activity from the second slider
  selected_activity_2 <- reactive({
    input$ActivitySlider2
  })
  
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
      lines(df$Smooth, lwd = 3, col = "red")
      
      #manually define the axis labels for the available data
      values_to_show <- seq(1, length(df$Smooth), length.out = 4)
      axis(1, at = values_to_show, labels = c("Aug ’18", "Nov ’19", "Mar ’21", "Jul ’22"), col.axis = "white")  # Set white axis color
      axis(2, col.axis = "white")
      mtext(text = "Daily Activity", side = 2, line = 3, col = "white", cex = 1.5)
      
    })
    
    #plot the simulation data
    output$SimulationPlot <- renderPlot({
      
      #determine the values to plot
      number <- postcode_statistics$Stations[postcode_statistics$Postcode == rv$postcode]
      activity <- selected_activity()
      capacity = capacity.simulation(number,activity,0.15,24,27)[3][[1]]
      max_capacity = capacity.simulation(number,activity,0.15,24,27)[4][[1]]
      
      par(mar=c(13,5,1.5,1), bg = "#636363", bty = "n")
 
      plot(capacity, type = "l", lwd = 3, col = "#828282", xlab = "", ylab = "",
          ylim = c(0, 1500),
          xlim = c(0,25))
      lines(max_capacity, lwd = 3, col = "red")
      
      #define the legend
      legend("topleft", legend = c("Bikes Docked", "Max Capacity of Postcode"),
            col = c("#828282", "red"), lty = 1, lwd = 1.5, cex = 0.98, box.lty =0)
      
      #define the axis formatting
      values_to_show <- seq(1, length(df$Smooth), length.out = 4)
      axis(1, col.axis = "white") 
      axis(2, col.axis = "white")
      mtext(text = "Bikes Docked", side = 2, line = 3, col = "white", cex = 1.25)
      mtext(text = "Hour", side = 1, line = 3, col = "white", cex = 1.5)
           
    })
    
    #render information about the short forecast model
    vars <- get(paste0(rv$postcode,".values"))
    rmse <- get(paste0(rv$postcode,".rmse"))
    shortforecast_observed <- get(paste0(rv$postcode,".test"))
    shortforecast_fc <- get(paste0(rv$postcode,".predict"))
    
    output$'intercept' <- renderText(round(vars[1],digits=2))
    output$'1day' <- renderText(round(vars[2],digits=2))
    output$'7day' <- renderText(round(vars[3],digits=2))
    output$'7dayav' <- renderText(round(vars[4],digits=2))
    output$'365dayav' <- renderText(round(vars[5],digits=2))
    output$'temp' <- renderText(round(vars[6],digits=2))
    output$'wind' <- renderText(round(vars[7],digits=2))
    output$'rain' <- renderText(round(vars[8],digits=2))
    output$rmse <- renderText(rmse)
    
    #plot the short term forecast
    output$Shortforecastplot <- renderPlot({
      par(mar=c(13,5,1.5,1), bg = "#636363", bty = "n")
      
      #plot the raw data in a light plot
      plot(shortforecast_observed$Daydemand, type = "l", lwd = 3, col = "#828282", xlab = "", ylab = "",
           ylim = c(0, 4000),
           xlim = c(1, length(shortforecast_observed$Daydemand)), xaxt = "n")
      lines(shortforecast_fc, lwd = 2, col = "red")
      
      #define the legend
      legend("bottomright", legend = c("Observed Activity", "Forecasted Activity"),
             col = c("#828282", "red"), lty = 1, lwd = 2, cex = 1.25, box.lty =0)
      
      #manually define the axis labels for the available data
      values_to_show <- seq(1, length(shortforecast_observed$Daydemand), length.out = 4)
      axis(1, at = values_to_show, labels = c("Feb ’22", "Apr ’22", "Jun ’22", "Aug ’22"), col.axis = "white")  # Set white axis color
      axis(2, col.axis = "white")
      mtext(text = "Daily Activity", side = 2, line = 3, col = "white", cex = 1.5)
      
    })
    
    #plot the long term forecast
    longforecast_observed <- get(paste0(rv$postcode,".obs"))
    longforecast_fc <- get(paste0(rv$postcode,".model"))
    
    output$Longforecastplot <- renderPlot({
      par(mar=c(13,5,1.5,1), bg = "#636363", bty = "n")
      
      #plot the raw data in a light plot
      plot(longforecast_observed, type = "l", lwd = 3, col = "#828282", xlab = "", ylab = "",
           ylim = c(0, 5000),
           xlim = c(1, length(longforecast_fc)), xaxt = "n")
      lines(longforecast_fc, lwd = 2, col = "red")
      
      #define the legend
      legend("bottomright", legend = c("Observed Activity", "Forecasted Activity"),
             col = c("#828282", "red"), lty = 1, lwd = 2, cex = 1.25, box.lty =0)
      
      #manually define the axis labels for the available data
      values_to_show <- seq(1, length(longforecast_fc), length.out = 4)
      axis(1, at = values_to_show, labels = c("Jul ’20", "Jan ’22", "Jun ’23", "Oct ’24"), col.axis = "white")  # Set white axis color
      axis(2, col.axis = "white")
      mtext(text = "Daily Activity", side = 2, line = 3, col = "white", cex = 1.5)
      
    })
    
    
    output$NewStations <- renderPlot({
      par(mar=c(13,5,1.5,1), bg = "#636363", bty = "n")
      
      # Example data
      categories <- c("EC1","EC2","EC3","EC4","WC1","WC2")
      values <- c(find_optimal(ec1.model[1000:length(ec1.model)],selected_activity_2())-n_stations[1],
                  find_optimal(ec2.model[1000:length(ec2.model)],selected_activity_2())-n_stations[2],
                  find_optimal(ec3.model[1000:length(ec3.model)],selected_activity_2())-n_stations[3],
                  find_optimal(ec4.model[1000:length(ec4.model)],selected_activity_2())-n_stations[4],
                  find_optimal(wc1.model[1000:length(wc1.model)],selected_activity_2())-n_stations[5],
                  find_optimal(wc2.model[1000:length(wc2.model)],selected_activity_2())-n_stations[6]
                  )
      
      
      
      #use the postcode labels to find the right bar to highlight for the current postcode
      highlighted_bar <- which(postcode_labels == rv$postcode)
      
      #create a barplot
      barplot(values, ylim = c(0, 30), col = ifelse(seq_along(categories) == highlighted_bar, "red", "#828282"), xlab ="", ylab = "", border = "NA", col.axis = "white", xaxt = "n")
      
      #manually define the axis labels for the available data
      values_to_show <- seq(0.75, 6.75, length.out = 6)
      axis(1, at = values_to_show, labels = c("EC1","EC2","EC3","EC4","WC1","WC2"), col.axis = "white")  # Set white axis color
      axis(2, col.axis = "white")
      mtext(text = "Needed Stations", side = 2, line = 3, col = "white", cex = 1.5)
      
      #update the slider to display the current optimal solution
      # Initialize a reactiveValues object to store the selected daily activity from the second slider
      
      n_stations <- values[highlighted_bar]
      updateSliderInput(session, "nstations", value = n_stations)
      model = get(paste0(rv$postcode,".model"))
      total_loss = loss_n_station(model[1000:length(model)],n_stations+20,selected_activity_2())
      print(total_loss)
      
      
    })
    
  })
  
}

# Launch the app
shinyApp(ui, server)
