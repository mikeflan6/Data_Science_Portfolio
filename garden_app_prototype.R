#Import Libraries
library(shiny)
library(ggplot2)
library(httr)
library(jsonlite)

#List of available plants 
plants <- c("Tomato", "Pepper", "Radish", "Lettuce", "Onion", "Carrot", "Pumpkin", "Watermellon",
            "Cucumber", "Sunflower", "Squash", "Chive", "Bean", "Basil", "Broccoli",
            "Chamomile", "Lavender", "Cilantro", "Potato", "Arugula", "Spinach", "Empty")

#Convert Zipcode to Lat/Lon coordinates using open-meteo api
#Create function "get_lat_lon"
get_lat_lon <- function(zipcode) {
  url <- paste0("https://geocoding-api.open-meteo.com/v1/search?name=", zipcode, "&count=1&language=en&format=json")
  response <- GET(url)   ##send request
  if (response$status_code != 200) return(NULL)   ##check request success!
  data <- fromJSON(content(response, "text"))
  if (length(data$results) == 0) return(NULL)   ##no response
  list(lat = data$results$latitude[1], lon = data$results$longitude[1])  ##return a list of first Lat/Lon result
}

#Weather API request
#Create funtion "get_weather"
get_weather <- function(zipcode, start_date, end_date) {   ##input:Zipcode, start_date, and end_date
  coords <- get_lat_lon(zipcode)   ##Call function "get_lat_lon"
  if (is.null(coords)) return("Invalid ZIP code or location not found.")   ##if null message
  
  url <- paste0(
    #open-meteo api URL
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", coords$lat,   ##Lat input
    "&longitude=", coords$lon,   ##Lon input
    "&start_date=", start_date,   ##Start date input
    "&end_date=", end_date,   ##End date input
    #return max temp at 2 meters above ground
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum",
    #use eastern standard time 
    "&timezone=America%2FNew_York"
  )
#Get request/Error message
  response <- GET(url)
  if (response$status_code != 200) return("Failed to retrieve weather data.")
#JSON to R list/Error message
  data <- fromJSON(content(response, "text"))
  if (length(data$daily$time) == 0) return("No weather data available for this location.")
#DF for open-meteo data/  
  df <- data.frame(
    Date = data$daily$time,
    #Convert Celsius to Fahrenheit
    Max_F = round(data$daily$temperature_2m_max * 9/5 + 32, 1),
    Min_F = round(data$daily$temperature_2m_min * 9/5 + 32, 1),
    #convert mm to in
    Precip_in = round(data$daily$precipitation_sum / 25.4, 2)
  )
  #combine results into single rows 
  paste0(apply(df, 1, function(row) {
    paste0(row[["Date"]], ": High ", row[["Max_F"]], "°F, Low ", row[["Min_F"]], "°F, Precip ", row[["Precip_in"]], " in")
  }), collapse = "\n")
}

#Setup UI
ui <- fluidPage(
  sidebarLayout(
    mainPanel(
      plotOutput("gardenPlot", height = "800px", width = "800px", click = "plot_click")
    ),
    sidebarPanel(
      tabsetPanel(
        tabPanel("Garden Data",   ##"Garden Data" tab
                 h4("View Plant Data"),
                 uiOutput("plantSelector"),
                 tableOutput("plantData"),
                 tags$hr(),
                 
                 h4("Garden Setup"),   ##"Garden Setup" header
                 #Input/Selection/Download/Upload buttons 
                 numericInput("rows", "Rows:", 10, min = 1),
                 numericInput("cols", "Columns:", 10, min = 1),
                 selectInput("plant", "Select a plant:", choices = plants),
                 dateInput("plantDate", "Planting Date:", value = Sys.Date()),
                 dateInput("harvestDate", "Harvest Date:", value = NA),
                 numericInput("yieldWeight", "Harvest Yield (lbs):", value = NA, min = 0, step = 0.1),
                 downloadButton("downloadGarden", "Download Garden"),
                 fileInput("loadGarden", "Load Garden", accept = ".rds")
        ),
        tabPanel("Weather Lookup",   ##"Weather Lookup" tab
                 h4("Weather Lookup"),    ##"Weather Lookup" header
                 #Input boxes
                 textInput("zipcode", "Enter ZIP Code:", "10001"),
                 dateInput("startDate", "Start Date:", value = Sys.Date() - 7),
                 dateInput("endDate", "End Date:", value = Sys.Date()),
                 #action button to get weather
                 actionButton("getWeather", "Get Weather"),
                 verbatimTextOutput("weatherInfo")   ##results of action button 
        )
      )
    )
  )
)

#Server
#Define server function 
server <- function(input, output, session) {
  #Build Garden Grid
  garden <- reactive({
    df <- expand.grid(x = 1:input$cols, y = 1:input$rows, plant = "Empty",
                      planting_date = as.Date(NA),
                      harvest_date = as.Date(NA),
                      yield_weight = as.numeric(NA))
    df$plant <- as.character(df$plant)
    df
  })
  #Reactive variable to store current state of Garden Grid
  selected_garden <- reactiveVal(NULL)
  #Update when grid change is made
  observe({
    selected_garden(garden())
  })
  
  observeEvent(input$plot_click, {
    #Extracts X/Y coordinates from plot click
    clicked_x <- round(input$plot_click$x)
    clicked_y <- round(input$plot_click$y)
    
    new_garden <- selected_garden()
    index <- which(new_garden$x == clicked_x & new_garden$y == clicked_y)
    if (length(index) > 0) {
      new_garden$plant[index] <- input$plant
      new_garden$planting_date[index] <- input$plantDate
      new_garden$harvest_date[index] <- input$harvestDate
      new_garden$yield_weight[index] <- input$yieldWeight
      selected_garden(new_garden)
    }
  })
#Garden Grid visualization   
  output$gardenPlot <- renderPlot({
    req(selected_garden())
    #Colors chosen for high contrast
    ggplot(selected_garden(), aes(x, y, fill = plant)) +
      geom_tile(color = "black") +
      scale_fill_manual(values = c(
        "Tomato"      = "tomato",        "Pepper"      = "springgreen",
        "Radish"      = "wheat1",        "Lettuce"     = "darkolivegreen1",
        "Onion"       = "cornsilk2",     "Carrot"      = "darkgoldenrod1",
        "Pumpkin"     = "darkorange",    "Watermellon" = "magenta",
        "Cucumber"    = "seagreen",      "Sunflower"   = "lightyellow1",
        "Squash"      = "purple",        "Chive"       = "thistle1",
        "Bean"        = "black",         "Basil"       = "deepskyblue",
        "Broccoli"    = "cyan",          "Chamomile"   = "yellow1",
        "Lavender"    = "lavender",      "Cilantro"    = "deeppink2",
        "Potato"      = "brown",         "Arugula"     = "steelblue",
        "Spinach"     = "cornsilk4",     "Empty"       = "white"
      )) +
      theme_minimal() +
      coord_fixed(ratio = 1) +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      #Add cardinal directions to Garden Grid
      labs(x = NULL, y = NULL) +
      annotate("text", x = mean(range(selected_garden()$x)), y = max(selected_garden()$y) + 1, label = "North", size = 6) +
      annotate("text", x = mean(range(selected_garden()$x)), y = min(selected_garden()$y) - 1, label = "South", size = 6) +
      annotate("text", x = max(selected_garden()$x) + 1.5, y = mean(range(selected_garden()$y)), label = "East", size = 6) +
      annotate("text", x = min(selected_garden()$x) - 1.5, y = mean(range(selected_garden()$y)), label = "West", size = 6)
  })
  #Download current garden
  output$downloadGarden <- downloadHandler(
    filename = function() {
      paste0("my_garden_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(selected_garden(), file)
    }
  )
  #Read in uploaded garden
  observeEvent(input$loadGarden, {
    req(input$loadGarden)
    loaded_data <- readRDS(input$loadGarden$datapath)
    selected_garden(loaded_data)
    showNotification("Garden loaded!", type = "message")
  })
  #Executes "get_weather" function given Zip, Start/End dates
  observeEvent(input$getWeather, {
    output$weatherInfo <- renderText({
      get_weather(input$zipcode, input$startDate, input$endDate)
    })
  })
  #Dropdown menu to choose plant from Garden Grid
  output$plantSelector <- renderUI({
    df <- selected_garden()
    if (is.null(df)) return(NULL)
    planted <- df[df$plant != "Empty", ]
    if (nrow(planted) == 0) return("No planted crops yet.")
    selectInput("selectedPlant", "Choose a plant from the garden:", 
                choices = unique(planted$plant))
  })
  #Render table of selected plant 
  output$plantData <- renderTable({
    df <- selected_garden()
    req(input$selectedPlant)
    filtered <- df[df$plant == input$selectedPlant, c("x", "y", "planting_date", "harvest_date", "yield_weight")]
    if (nrow(filtered) == 0) return(NULL)
    
    #Format dates for clarity
    filtered$planting_date <- as.character(filtered$planting_date)
    filtered$harvest_date <- as.character(filtered$harvest_date)
    
    filtered
  })
}

#Run the app
shinyApp(ui, server)
