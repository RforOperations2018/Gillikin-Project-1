# Project 1
# Shiny Dashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)


#load data downloaded from Southern Poverty Law Center
#https://docs.google.com/spreadsheets/d/17ps4aqRyaIfpu7KdGsy2HRZaaQiXUfLrpUbaR9yS51E/edit?usp=sharing
monuments.load <- read.csv("monuments.csv")

header <- dashboardHeader(title = "153 Years Later"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
      radioButtons("statusInput", 
                 label = h3("Status"),
                 choices = list("Active" = 1, "Removed" = 2), 
                 selected = 1),
                 fluidRow(column(3, verbatimTextOutput("value"))),
      selectInput("stateInput",
                 label = h3("Select state(s)"),
                 choices = sort(unique(monuments.load$state)),
                 multiple = TRUE),
      selectInput("statusInput",
                label = h3("Select status(s)"),
                choices = sort(unique(monuments.load$status)),
                multiple = TRUE),
      dateRangeInput("yearInput", 
                label = h3("Years"),
                start = "1865-05-13", 
                end = as.character(Sys.Date()),
                format = "yyyy"),
    sliderInput("Year", "Year released", 1850, 2018, c(1850, 2018))
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            # Count valuebox
            valueBoxOutput("count"),
            # States valuebox
            valueBoxOutput("states"),
            # Percent valuebox
            valueBoxOutput("percent")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   # Year monument erected plot
                   tabPanel("Year Erected", plotlyOutput("plot_state")),
                   # Side state was on in the Civil War plot
                   tabPanel("Side in Civil War", plotlyOutput("plot_side")),
                   # Type of monument plot
                   tabPanel("Type of Monument", plotlyOutput("plot_type")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Monument Statistics", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  mmInput <- reactive({
    monuments <- monuments.load 
    #monuments <- monuments[(monuments.load$year.dedicated >= input$yearInput[1]) & (monuments.load$year.dedicated <= input$yearInputr[2]), ]
                                
    #[year.dedicated >= input$yearInput[1] & year.dedicated <= input$yearInput[2], ]
    #my_data <- my_data[(my_data$Year>=input$Year[1]) & (my_data$Year<=input$Year[2]), ]
    if (length(input$stateInput) > 0 ) {
      monuments <- subset(monuments, state %in% input$stateInput)
    }
    #if (length(input$statusInput) > 0 ) {
    #  monuments <- subset(monuments, status %in% input$statusInput)
    #}
    return(monuments)
  })
  # Monuments by year and total by state
  output$plot_state <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.dedicated, fill = state)) + 
        geom_bar(type=input$statusInput)
    ) 
  })
  # Monuments by side
  output$plot_side <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.dedicated, fill = side)) + 
        geom_bar()
    )
  })
  # Monuments by type
  output$plot_type <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = state, fill = type)) + 
        geom_bar(position = "stack")
    )
  })
  # Data table of monuments
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(state, side, type, year.dedicated, year.removed, status))
  })
  # Count value box
  output$count <- renderValueBox({
    mm <- mmInput()
    num <- round(mean(mm$state, na.rm = T), 2)
    valueBox("Confederate Monuments", value = paste0(nrow(mm)), color = "blue")
  })
  # Number of states value box
  output$states <- renderValueBox({
    mm <- mmInput()
    num <- count(mm, state)
    valueBox("Number of States", value = paste0(num), color = "olive")
  })
  # Percent value box
  output$percent <- renderValueBox({
    mm <- mmInput()
    num <- round(nrow(mm)/nrow(monuments.load), 4) * 100 
    valueBox("of total", value = paste0(num,"%"), color = "teal")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)