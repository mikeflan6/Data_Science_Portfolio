# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(shinydashboard)
library(bslib)


#Read in csv file
HT_DB <- read.csv("Happy_Tree_DB_13-23.csv")

# Define UI 
#creates fluid layout which adjusts to fit browser window size.
#applies bootstrap theme "lux".
#creates title panel.
ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  titlePanel("Trees"),

#splits UI into side panel and main panel.
  sidebarLayout(
#creates section for elements inside side panel.
    sidebarPanel(
#creates drop down menu for State,County, and Top Tree
      selectInput("state", label = h3("Select State for Map 1"), 
                  choices = unique(HT_DB$STATE), 
                  selected = unique(HT_DB$STATE)[1]),
      selectInput("county", label = h3("Select County for Map 1"), 
                  choices = NULL), # initially empty
      selectInput("species", label = h3("Trees in State/County for Map 2"), 
                  choices = NULL),
#action button for randomizing choices in the 3 drop down menus.
      actionButton("random", "Randomize"),
#line break to add space between UI elements
      tags$br(), tags$br(),
#value boxes displaying hardiness zone, top tree, and total trees within selected State/County
      valueBoxOutput("hardinessZone", width = 12),
      tags$br(), tags$br(),
      valueBoxOutput("topTree", width = 12),
      tags$br(), tags$br(),
      valueBoxOutput("totalTrees", width = 12),
      tags$br(), tags$br(),
#pie chart placement
      plotOutput("pieChart"),
    ),

#creates section for elements inside side panel.
    mainPanel(
#map 1 placement
      plotOutput("mapPlot"),
      tags$br(), tags$br(),
      tags$br(), tags$br(),
      tags$br(), tags$br(),
      tags$br(), tags$br(),
      tags$br(), tags$br(),
#map 2 placement
      plotOutput("mapPlot2")
    )
  )
)

#Define server 
#server contains 3 arguments (input,output,and session)
server <- function(input, output, session) {

#observes input from state drop down menu
#filters data frame for counties within chosen state
#updates county drop down menu to include all counties within chosen state
  observeEvent(input$state, {
    selected_state <- input$state
    counties <- HT_DB %>%
      filter(STATE == selected_state) %>%
      select(COUNTYNM) %>%
      distinct() %>%
      pull(COUNTYNM)
    updateSelectInput(session, "county", choices = counties)
  })

#observes input from county drop down menu
#filters data frame for trees (common name) within chosen State/County
#updates trees drop down menu to include common name of all trees within chosen State/County
  observeEvent(input$county, {
    selected_state <- input$state
    selected_county <- input$county
    species <- HT_DB %>%
      filter(STATE == selected_state & COUNTYNM == selected_county) %>%
      select(COMMON_NAME) %>%
      distinct() %>%
      pull(COMMON_NAME)
    updateSelectInput(session, "species", choices = species)
  })

#observes when RANDOMIZE button is clicked
#draws a sample from State,County within State,and Tree common name within State/County
  observeEvent(input$random, {
    random_state <- sample(unique(HT_DB$STATE), 1)
    random_county <- HT_DB %>%
      filter(STATE == random_state) %>%
      select(COUNTYNM) %>%
      distinct() %>%
      sample_n(1) %>%
      pull(COUNTYNM)
    random_species <- HT_DB %>%
      filter(STATE == random_state & COUNTYNM == random_county) %>%
      select(COMMON_NAME) %>%
      distinct() %>%
      sample_n(1) %>%
      pull(COMMON_NAME)
#updates drop down menus to include randomly chosen inputs    
    updateSelectInput(session, "state", selected = random_state)
    updateSelectInput(session, "county", selected = random_county)
    updateSelectInput(session, "species", selected = random_species)
  })
  
#generates text for chosen State/County  
  output$value <- renderText({ 
    paste("State/County:", input$state, "->", input$county)
  })

#defines reactive plot output for map 1  
#ensures County is chosen
  output$mapPlot <- renderPlot({
    req(input$county) 
    
#filters data frame for chosen State/County    
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)

#creates map 1 (US map)
#places point on map from longitude and latitude = State/County
#adjustments for point size, title size, and map size
#theme void to remove grid
    us_map <- map_data("state")
    ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                   fill = "white", color = "black") +
      geom_point(data = selected_data, aes(x = long, y = lat), 
                 color = "red", size = 3) +
      coord_fixed(1.3) +
      labs(title = paste("Map 1", input$county, "County,", input$state),
           x = "", y = "") +
      theme_void() +
      theme(plot.title = element_text(size = 20), 
            axis.title = element_text(size = 16)) 
  }, width = 1000, height = 700)

#defines reactive plot output for map 2 
#ensures common name is chosen  
  output$mapPlot2 <- renderPlot({
    req(input$species) 

#filters data frame for chosen Tree common name    
    selected_data <- HT_DB %>%
      filter(COMMON_NAME == input$species)

#creates map 2 (US map)
#places points on map from longitude and latitude = State/County where selected tree is found
#adjustments for point size, title size, and map size
#theme void to remove grid    
    us_map <- map_data("state")
    ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                   fill = "white", color = "black") +
      geom_point(data = selected_data, aes(x = long, y = lat), 
                 color = "lightblue", size = 0.5) +
      coord_fixed(1.3) +
      labs(title = paste("Map 2", input$species, "Locations"),
           x = "", y = "") +
      theme_void() +
      theme(plot.title = element_text(size = 20), 
            axis.title = element_text(size = 16)) 
  }, width = 1000, height = 700)
  
#defines reactive text output for most frequent common name within chosen State/County 
#requires State/County is chosen
  output$mostFrequentCommonName <- renderText({
    req(input$county) 
    
#filters data frame for chosen State/County 
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)

#Counts occurrences of tree common name within selected data and pulls the most frequent     
    most_frequent <- selected_data %>%
      count(COMMON_NAME) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(COMMON_NAME)
    most_frequent
  })

#creates a text output for top tree in selected State/County
#requires State/County is chosen
  output$percentageCommonName <- renderText({
    req(input$county) 

#filters data frame for chosen State/County     
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)
    
#counts the frequency of top tree from chosen State/County and calculates its overall percentage of trees within selected State/County    
    total_count <- nrow(selected_data)
    most_frequent_count <- selected_data %>%
      count(COMMON_NAME) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(n)
#rounds percentage of top tree to 2 decimal places    
    percentage <- round((most_frequent_count / total_count) * 100, 2)
    paste(percentage, "%")
  })
 
#defines reactive output for pie chart
#requires State/County is chosen
  output$pieChart <- renderPlot({
    req(input$county)

#filters data frame for chosen State/County     
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)

#counts and identifies most frequent common name from chosen State/County    
    most_frequent <- selected_data %>%
      count(COMMON_NAME) %>%
      arrange(desc(n)) %>%
      slice(1)

#total rows for chosen State/County    
    total_count <- nrow(selected_data)
    most_frequent_count <- most_frequent$n
    other_count <- total_count - most_frequent_count

#creates a data frame with data for pie chart    
    pie_data <- data.frame(
      category = c(as.character(most_frequent$COMMON_NAME), "Others"),
      count = c(most_frequent_count, other_count)
    )

#calculates percentage of top tree within chosen State/County    
    pie_data <- pie_data %>%
      mutate(percentage = round((count / total_count) * 100, 1)) %>% 
      mutate(label = paste0(percentage, "%"))

#creates pie chart
#adds data based on calculations for pie data
#adjustments for title size and label size
#theme void to remove grid
    ggplot(pie_data, aes(x = "", y = count, fill = category)) +
      geom_bar(stat = "identity", width = 1, color="white") +
      coord_polar(theta = "y") +
      labs(title = "Percentage of Top Tree in State/County", x = "", y = "") +
      theme_void() +
      geom_text(aes(y = count, label = label), position = position_stack(vjust = 0.5), color = "black", size = 6) + # Increased label size
      theme(plot.title = element_text(size = 24)) 
  })
  
#creates value box to display hardiness zone
#requires State/County is chosen 
  output$hardinessZone <- renderValueBox({
    req(input$county) 

#filters data frame for chosen State/County 
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)

#extracts unique hardiness zone from chosen State/County
#adds snowflake since hardiness zone refers to freezing points 
#green
    zone <- unique(selected_data$zonetitle)
    valueBox(zone, "Hardiness Zone", icon = icon("snowflake"), color = "green")
  })

#creates value box to display top tree
#requires State/County is chosen 
  output$topTree <- renderValueBox({
    req(input$county) 

#filters data frame for chosen State/County 
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)

#extracts most frequent tree common name from chosen State/County    
    top_tree <- selected_data %>%
      count(COMMON_NAME) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(COMMON_NAME)

#adds tree because tree
#green because trees=green
    valueBox(top_tree, "Top Tree", icon = icon("tree"), color = "green")
  })
 
#creates value box to display total tree 
#requires State/County is chosen  
  output$totalTrees <- renderValueBox({
    req(input$county) # Ensure county is selected before calculating

#filters data frame for chosen State/County 
    selected_data <- HT_DB %>%
      filter(STATE == input$state & COUNTYNM == input$county)
    
#adds total rows of selected State/County    
    total_trees <- nrow(selected_data)
    
#adds tree 
#green because all the other value boxes were green so why not
    valueBox(total_trees, "Total Trees", icon = icon("tree"), color = "green")
  })
}

#Run the app and pray it works
shinyApp(ui = ui, server = server)
