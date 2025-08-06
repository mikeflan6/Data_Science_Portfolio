#Import Libraries
library(dplyr)
library(ggplot2)
library(rsconnect)
library(shiny)
library(httr)
library(jsonlite)
library(bslib)
library(shinyWidgets)
library(DT)
library(tidyr)

#List of available plants 
plants <- c(
  "Apple", "Avocado", "Banana", "Bean", "Blueberry", "Broccoli", "Carrot",
  "Chamomile", "Cherry", "Corn", "Cucumber", "Eggplant", "Garlic",
  "Grape", "Lavender", "Lemon", "Lettuce", "Olive", "Onion", "Orange",
  "Peach", "Pear", "Peas", "Pepper (hot)", "Pepper (sweet)",
  "Pineapple", "Potato", "Pumpkin", "Strawberry", "Sunflower", "Tomato",
  "Watermelon", "Other", "Empty"
)

#Convert Zipcode to Lat/Lon coordinates using open-meteo api

#Create function "get_lat_lon" with input "zipcode"
get_lat_lon <- function(zipcode) {
#API request URL. Returns 01 result in english in json format
  url <- paste0("https://geocoding-api.open-meteo.com/v1/search?name=", zipcode, "&count=1&language=en&format=json")
#HTTP GET request
  response <- GET(url)
#Check request success
  if (response$status_code != 200) return(NULL)
#Parse the JSON response
  data <- fromJSON(content(response, "text"))
#Check if results were returned 
  if (length(data$results) == 0) return(NULL) 
#Return lat and lon of first matching location 
  list(lat = data$results$latitude[1], lon = data$results$longitude[1])  
}

#Weather API request

#Create funtion "get_weather" with inputs "zipcode", "start_date", "end_date"
get_weather <- function(zipcode, start_date, end_date) {
#Calls previous "get_lat_lon" function
  coords <- get_lat_lon(zipcode)
#return null if coordinates can not be retrieved 
  if (is.null(coords)) return(list(summary = "Invalid ZIP code or location not found.", data = NULL))
#API request. requests daily min/max temp and precipitation. sets timezone to eastern   
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", coords$lat,
    "&longitude=", coords$lon,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum",
    "&timezone=America%2FNew_York"
  )
#Sends GET request to weather archive API  
  response <- GET(url)
#Check request success
  if (response$status_code != 200) return(list(summary = "Failed to retrieve weather data.", data = NULL))
#Parse JSON response into R object   
  data <- fromJSON(content(response, "text"))
#Check if daily data was returned 
  if (length(data$daily$time) == 0) return(list(summary = "No weather data available for this location.", data = NULL))
#Converts celsius to fahrenheit and mm to in
  df <- data.frame(
    Date = data$daily$time,
    Max_F = round(data$daily$temperature_2m_max * 9/5 + 32, 1),
    Min_F = round(data$daily$temperature_2m_min * 9/5 + 32, 1),
    Precip_in = round(data$daily$precipitation_sum / 25.4, 2)
  )
#Generate readable summary string for each day  
  summary_text <- paste0(apply(df, 1, function(row) {
    paste0(row[["Date"]], ": High ", row[["Max_F"]], "°F, Low ", row[["Min_F"]], "°F, Precip ", row[["Precip_in"]], " in")
  }), collapse = "\n")
#Return summary and data frame   
  return(list(summary = summary_text, data = df))
}
######################################################################################################################################################
#DO NOT CHANGE ABOVE LINE
######################################################################################################################################################

# Setup UI
#Define UI layout using fluidpage
ui <- fluidPage(
#Set theme and font
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#4CAF50",
    bg = "#e8f5e9",
    fg = "black",
    base_font = font_google("Lato")
  ),
#UI Block 01: Logo
#Display "Garden_App" logo at the top of app layout
  fluidRow(
    column(
      width = 12,
      tags$div(
        style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
        tags$img(src = "Garden_App_Logo.png", style = "width: 150px;")
      )
    )
  ),
#UI Block 02: Garden View
#Tab for garden plot with click interaction 
  tabsetPanel(
    tabPanel("Garden View", icon = icon("seedling"),
             fluidRow(
               column(
                 width = 8,
                 tags$div(
                   id = "gardenGrid",
                   class = "p-2",
                   style = "background-color: transparent;",
                   plotOutput("gardenPlot", height = "800px", width = "100%", click = "plot_click")
                 )
               ),
               column(
                 width = 4,
                 tags$div(
                   style = "margin-top: 10px;",
#UI Block 03: Plant Details 
#Tab for entering plant specific data
                   tabsetPanel(
                     tabPanel("Plant Details", icon = icon("leaf"),
                              pickerInput("plant", "Choose your plant:", choices = plants),
                              dateInput("plantDate", "Planting Date:", value = Sys.Date()),
                              selectInput("fertFreq", "Fertilizer Frequency:", choices = 1:5, selected = 1),
                              uiOutput("fertilizerInputs"),
                              numericInput("numHarvests", "Number of Harvests:", value = 1, min = 1, max = 10),
                              uiOutput("harvestInputs"),
                              sliderInput("Full_Sun_Exposure", "Full Sun Exposure (hrs.):", min = 1, max = 24, value = 12),
                              sliderInput("Water_Schedule", "Water Schedule (mins/day):", min = 0, max = 120, value = 30)
                     ),
#UI Block 04: Garden Setup
#Tab for adjusting garden size, downloading, and uploading garden
                     tabPanel("Garden Setup", icon = icon("tools"),
                              h4("Garden Setup"),
                              sliderTextInput("rows", "Rows:", choices = as.character(1:10), selected = "10"),
                              sliderTextInput("cols", "Columns:", choices = as.character(1:10), selected = "10"),
                              downloadButton("downloadGarden", "Download Garden"),
                              fileInput("loadGarden", "Load Garden", accept = ".rds")
                     )
                   )
                 )
               )
             )
    ),
#UI Block 05: Plant Data
#Tab for viewing plant data input into garden
    tabPanel("Plant Data", icon = icon("tree"),
             fluidRow(
               column(
                 width = 12,
                 h4("View Plant Data"),
                 uiOutput("plantSelector"),
                 tableOutput("plantData")
               )
             )
    ),
#UI Block 06: Weather Lookup
#Tab for retrieving weather data using Zip code and date range 
    tabPanel("Weather Lookup", icon = icon("cloud-sun"),
             fluidRow(
               column(
                 width = 12,
                 h4("Weather Lookup"),
                 textInput("zipcode", "Enter ZIP Code:", "10001"),
                 dateInput("startDate", "Start Date:", value = Sys.Date() - 7),
                 dateInput("endDate", "End Date:", value = Sys.Date()),
                 actionButton("getWeather", "Get Weather"),
                 verbatimTextOutput("weatherInfo")
               )
             )
    ),
#UI Block 07: Database
#Tab for viewing garden data saved to database 
    tabPanel("Database", icon = icon("database"),
             fluidRow(
               column(
                 width = 12,
                 h3("Database"),
                 tags$div(
                   style = "overflow-x: auto;",
                   DT::dataTableOutput("garden_table")
                 )
               )
             )
    ),
#UI Block 08: Analytics
#Tab for exploring garden data from Database
    tabPanel("Analytics", icon = icon("chart-line"),
             h4("Explore Your Garden Data"),
             fluidRow(
               column(4,
                      pickerInput("filterPlant", "Select Plant Type:", choices = character(0), multiple = TRUE),
                      dateRangeInput("filterDate", "Select Date Range:",
                                     start = as.Date("2019-04-01"), end = Sys.Date())
               ),
               column(8,
                      tabsetPanel(
                        tabPanel("Harvest Trends", icon = icon("chart-area"),
                                 plotOutput("totalHarvestPlot")
                        ),
                        tabPanel("Yield Prediction", icon = icon("calculator"),
                                 tags$div(
                                   style = "margin-top: 10px;",
                                   plotOutput("yieldPredictionPlot", height = "400px")
                                 )
                        )
                      )
               )
             )
    ),
#UI Block 09: Instructions 
#Tab for displaying app instructions with images 
    tabPanel("Instructions", icon = icon("book"),
             h4("Instructions"),
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
                   uiOutput("instruction_image")  
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   style = "text-align: center; margin-bottom: 30px;",
                   actionButton("previous_img", label = "← Previous"),
                   actionButton("next_img", label = "Next →")
                 )
               )
             )
    )
    )
  )
#########################################################################################################################################################

#Server
#input (input values from UI)
#output (send reactive result back to UI)
#session (provides access to session-specific information)
server <- function(input, output, session) {

#server block 01: Synthetic data for Database    
#reactive expression to read in synthetic garden data as RDS file 
  garden_data <- reactive({
    readRDS("synthetic_garden.rds")
  })

#server block 02: Database to show synthetic data  
#renders datatable to display synthetic garden data
  output$garden_table <- DT::renderDataTable({
    DT::datatable(
      garden_data() %>%
        select(
          year, x, y, plant, planting_date,
          Full_Sun_Exposure, Water_Schedule,
          Harvest_Date, Yield_grams,
          Fert_Date, N, P, K
        ),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      class = "display nowrap",
      rownames = FALSE
    )
  })
  
#server block 03
#generates UI inputs for fertilizer schedules   
  output$fertilizerInputs <- renderUI({
    freq <- as.numeric(input$fertFreq)
    if (is.na(freq) || freq == 0) return(NULL)
    tagList(
      lapply(1:freq, function(i) {
        fluidRow(
          column(width = 3,
                 dateInput(paste0("fertDate_", i), label = paste("Date", i), value = Sys.Date())
          ),
          column(width = 3,
                 numericInput(paste0("N_", i), label = "N (Nitrogen)", value = NA, min = 0)
          ),
          column(width = 3,
                 numericInput(paste0("P_", i), label = "P (Phosphorus)", value = NA, min = 0)
          ),
          column(width = 3,
                 numericInput(paste0("K_", i), label = "K (Potassium)", value = NA, min = 0)
          )
        )
      })
    )
  })

#server block 04  
#generates UI inputs for harvests
  output$harvestInputs <- renderUI({
    req(input$numHarvests)
    
    harvest_ui <- lapply(1:input$numHarvests, function(i) {
      tagList(
        dateInput(paste0("harvestDate_", i), paste("Harvest Date", i, ":"), value = Sys.Date()),
        numericInput(paste0("yieldWeight_", i), paste("Yield (grams)", i, ":"), value = NA, min = 0, step = 1)
      )
    })
    
    do.call(tagList, harvest_ui)
  })

#server block 05  
#Build Garden Grid
  garden <- reactive({
    df <- expand.grid(x = 1:input$cols, y = 1:input$rows, plant = "Empty",
                      planting_date = as.Date(NA),
                      Full_Sun_Exposure = as.numeric(NA),
                      Water_Schedule = as.numeric(NA),
                      stringsAsFactors = FALSE)
    
    df$plant <- as.character(df$plant)
    df$harvests <- vector("list", length = nrow(df))    
    df$fertilizers <- vector("list", length = nrow(df))  
    df
  })

#server block 06
#reactive values to store garden and weather data  
  selected_garden <- reactiveVal(NULL)
  weather_df <- reactiveVal(NULL)

#server block 07
#sets initial value of selected_garden to the reactive garden grid when the app starts  
  observe({
    selected_garden(garden())
  })

#server block 08
#handles clicks on the garden
  observeEvent(input$plot_click, {
    clicked_x <- round(input$plot_click$x)
    clicked_y <- round(input$plot_click$y)
    new_garden <- selected_garden()
    index <- which(new_garden$x == clicked_x & new_garden$y == clicked_y)
    if (length(index) > 0) {
      # 🌾 Collect harvest data
      num <- input$numHarvests
      harvest_dates <- sapply(1:num, function(i) input[[paste0("harvestDate_", i)]])
      harvest_weights <- sapply(1:num, function(i) input[[paste0("yieldWeight_", i)]])
      
      harvest_df <- data.frame(
        Harvest = seq_len(num),
        Date = as.Date(harvest_dates),
        Weight_grams = as.numeric(harvest_weights)
      )
      freq <- as.numeric(input$fertFreq)
      fert_df <- NULL
      if (!is.na(freq) && freq > 0) {
        fert_df <- data.frame(
          Date = as.Date(sapply(1:freq, function(i) input[[paste0("fertDate_", i)]])),
          N = sapply(1:freq, function(i) input[[paste0("N_", i)]]),
          P = sapply(1:freq, function(i) input[[paste0("P_", i)]]),
          K = sapply(1:freq, function(i) input[[paste0("K_", i)]])
        )
      }
      new_garden$plant[index] <- input$plant
      new_garden$planting_date[index] <- input$plantDate
      new_garden$Full_Sun_Exposure[index] <- input$Full_Sun_Exposure
      new_garden$Water_Schedule[index] <- input$Water_Schedule
      new_garden$harvests[[index]] <- harvest_df
      new_garden$fertilizers[[index]] <- fert_df
      
      selected_garden(new_garden)
    }
  })

#server block 09
#emoji map for each unique plant  
  emoji_map <- c(
    "Apple" = "🍎", "Avocado" = "🥑", "Banana" = "🍌", "Bean" = "🫘",
    "Blueberry" = "🫐", "Broccoli" = "🥦", "Carrot" = "🥕", "Chamomile" = "🌼",
    "Cherry" = "🍒", "Corn" = "🌽", "Cucumber" = "🥒", "Eggplant" = "🍆",
    "Garlic" = "🧄", "Grape" = "🍇", "Lavender" = "🪻", "Lemon" = "🍋",
    "Lettuce" = "🥬", "Olive" = "🫒", "Onion" = "🧅", "Orange" = "🍊",
    "Peach" = "🍑", "Pear" = "🍐", "Peas" = "🫛", "Pepper (hot)" = "🌶️",
    "Pepper (sweet)" = "🫑", "Pineapple" = "🍍", "Potato" = "🥔",
    "Pumpkin" = "🎃", "Strawberry" = "🍓", "Sunflower" = "🌻",
    "Tomato" = "🍅", "Watermelon" = "🍉", "Other" = "🌱", "Empty" = "⬜"
  )

#server block 10
#renders the garden grid using ggplot2  
  output$gardenPlot <- renderPlot({
    df <- selected_garden()
    df$emoji <- emoji_map[df$plant]
    ggplot(df, aes(x, y)) +
      geom_text(aes(label = emoji), size = 14) +
      coord_fixed() +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0, 0, 0, 0, unit = "pt")
      ) +
      labs(x = NULL, y = NULL) +
      annotate("text", x = mean(range(df$x)), y = max(df$y) + 1, label = "North", size = 6) +
      annotate("text", x = mean(range(df$x)), y = min(df$y) - 1, label = "South", size = 6) +
      annotate("text", x = max(df$x) + 1.5, y = mean(range(df$y)), label = "East", size = 6) +
      annotate("text", x = min(df$x) - 1.5, y = mean(range(df$y)), label = "West", size = 6)
  })
 
#server block 11 
#download garden plot
  output$downloadGarden <- downloadHandler(
    filename = function() {
      paste0("my_garden_", Sys.Date(), ".rds")
    },
    content = function(file) {
      garden_data <- selected_garden()
      weather_data <- weather_df()
      saveRDS(list(garden = garden_data, weather = weather_data), file)
    }
  )

#server block 12  
#upload garden plot
  observeEvent(input$loadGarden, {
    req(input$loadGarden)
    data <- readRDS(input$loadGarden$datapath)
    selected_garden(data$garden)
    weather_df(data$weather)
    showNotification("Garden loaded from RDS!", type = "message")
  })

#server block 13  
#executes "get_weather" function given Zip, Start/End dates
  observeEvent(input$getWeather, {
    result <- get_weather(input$zipcode, input$startDate, input$endDate)
    output$weatherInfo <- renderText({ result$summary })
    if (!is.null(result$data)) {
      weather_df(result$data)
    }
  })

#server block 14  
#dropdown menu to choose plant from Garden Grid
  output$plantSelector <- renderUI({
    df <- selected_garden()
    if (is.null(df)) return(NULL)
    planted <- df[df$plant != "Empty", ]
    if (nrow(planted) == 0) return("No planted crops yet.")
    selectInput("selectedPlant", "Choose a plant from the garden:",
                choices = unique(planted$plant))
  })

#server block 15
#generates a summary table 
  output$plantData <- renderTable({
    df <- selected_garden()
    req(input$selectedPlant)
    filtered <- df[df$plant == input$selectedPlant, ]
    if (nrow(filtered) == 0) return(NULL)
    full_list <- lapply(seq_len(nrow(filtered)), function(i) {
      harvests <- filtered$harvests[[i]]
      if (is.null(harvests) || nrow(harvests) == 0) {
        harvests <- data.frame(Harvest = NA, Date = NA, Weight_grams = NA)
      }
      fertilizers <- filtered$fertilizers[[i]]
      if (is.null(fertilizers) || nrow(fertilizers) == 0) {
        fertilizers <- data.frame(Date = NA, N = NA, P = NA, K = NA)
      }
      max_rows <- max(nrow(harvests), nrow(fertilizers))
      harvests <- harvests[rep(1:nrow(harvests), length.out = max_rows), ]
      fertilizers <- fertilizers[rep(1:nrow(fertilizers), length.out = max_rows), ]
      data.frame(
        x = filtered$x[i],
        y = filtered$y[i],
        Plant = filtered$plant[i],
        Planting_Date = as.character(filtered$planting_date[i]),
        Sun_Exposure_hrs = filtered$Full_Sun_Exposure[i],
        Water_Schedule_mins = filtered$Water_Schedule[i],
        Harvest_Number = harvests$Harvest,
        Harvest_Date = as.character(harvests$Date),
        Yield_grams = harvests$Weight_grams,
        Fert_Date = as.character(fertilizers$Date),
        N = fertilizers$N,
        P = fertilizers$P,
        K = fertilizers$K
      )
    })
    do.call(rbind, full_list)
  })

# Server Block 16
# Analytics tab
  
#Update plant choices dynamically from database
  observe({
    req(garden_data())
    updatePickerInput(inputId = "filterPlant", choices = as.character(unique(garden_data()$plant)))
  })
  
#Filtered data from database
  filtered_data <- reactive({
    df <- garden_data()
    req(df)
    
#Filter by plant type
    if (!is.null(input$filterPlant) && length(input$filterPlant) > 0) {
      df <- df %>% filter(plant %in% input$filterPlant)
    }
    
#Filter by planting date
    if (!is.null(input$filterDate)) {
      df <- df %>%
        filter(planting_date >= input$filterDate[1],
               planting_date <= input$filterDate[2])
    }
    
    df
  })
  
#Total Harvest Plot (Harvest Yield Over Time)
  output$totalHarvestPlot <- renderPlot({
    df <- filtered_data()
    req(df)
    
    df %>%
      group_by(plant, Harvest_Date) %>%
      summarise(total_harvest = sum(Yield_grams, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = Harvest_Date, y = total_harvest, color = plant)) +
      geom_line(size = 1.2) +
      labs(title = "Total Harvest Over Time", x = "Harvest Date", y = "Yield (grams)") +
      theme_minimal()
  })
  
#Yield Prediction Plot (Year-aware, single-plant compatible)
  output$yieldPredictionPlot <- renderPlot({
    df <- filtered_data()
    req(df)
    
    df <- df %>%
      mutate(
        year = lubridate::year(Harvest_Date),
        days_since_planting = as.numeric(Harvest_Date - planting_date)
      )
    
#Ensure there are at least 2 years of data
    if (length(unique(df$year)) < 2) {
      plot.new()
      title("Yield prediction requires data from at least 2 different years.")
      return()
    }
    
#Fit model with year included
    model <- lm(Yield_grams ~ year + Full_Sun_Exposure + Water_Schedule + days_since_planting, data = df)
    
    df$predicted_yield <- predict(model, newdata = df)
    
    ggplot(df, aes(x = Yield_grams, y = predicted_yield, color = plant)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Actual vs Predicted Yield", x = "Actual Yield (grams)", y = "Predicted Yield (grams)") +
      theme_minimal()
  })
#List of image file names in www/ directory for "Instruction" tab
  instruction_files <- c(
    "Garden_App_Instructions_01_text.png",
    "Garden_App_Instructions_03_text.png",
    "Garden_App_Instructions_02_text.png",
    "Garden_App_Instructions_04_text.png",
    "Garden_App_Instructions_05_text.png",
    "Garden_App_Instructions_07_text.png",
    "Garden_App_Instructions_08_text.png",
    "Garden_App_Instructions_09_text.png"
  )
  
#Reactive image index
  current_index <- reactiveVal(1)
  
#Handle "Previous ←" button
  observeEvent(input$previous_img, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
    }
  })
#Handle "Next →" button
  observeEvent(input$next_img, {
    if (current_index() < length(instruction_files)) {
      current_index(current_index() + 1)
    }
  })
  
#Render image
  output$instruction_image <- renderUI({
    tags$img(
      src = instruction_files[current_index()],
      style = "width: 80%; border-radius: 8px;"
    )
  })

}
#Run the app
shinyApp(ui, server)