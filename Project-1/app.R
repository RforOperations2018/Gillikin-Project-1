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

header <- dashboardHeader(title = "153 Years Later: Civil War Monuments in the U.S."
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
      radioButtons("statusInput", 
                 label = h3("Status"),
                 choices = list("Added" = 1, "Removed" = 2), 
                 selected = 1),
                 fluidRow(column(3, verbatimTextOutput("value"))),
      selectInput("stateInput",
                 label = h3("Select state(s)"),
                 choices = sort(unique(monuments.load$state)),
                 multiple = TRUE),
      sliderInput("yearInput",
                label = h3("Years since Civil War"),
                min = 0,
                max = 153,
                value = c(0,153),
                step = 1)
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            #infoBoxOutput("state")#,
            valueBoxOutput("state")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Year Erected", plotlyOutput("plot_state")),
                   tabPanel("Side", plotlyOutput("plot_side")),
                   tabPanel("Monument Type", plotlyOutput("plot_type")))
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
    # Slider Year Filter
    #filter(2018 - year >= input$yearInput[1] & 2018 - year <= input$yearInput[2])
    # State Filter
    if (input$statusInput == "Added" ) {
      monuments <- subset(monuments, status %in% input$statusInput)
    }
    if (length(input$stateInput) > 0 ) {
      monuments <- subset(monuments, state %in% input$stateInput)
    }
    return(monuments)
  })
  # Monuments by year and total by state
  output$plot_state <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.added, fill = state)) + 
        geom_bar()
    ) 
  })
  # Monuments by side
  output$plot_side <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.added, fill = side)) + 
        geom_bar()
    )
  })
  # Monuments by type
  output$plot_type <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.added, fill = type)) + 
        geom_bar()
    )
  })
  # Data table of monuments
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(state, side, type, year.added, number))
  })
  # Sum of total
  output$state <- renderValueBox({
    mm <- mmInput()
    num <- mean(mm$state, na.rm = FALSE)
    valueBox("Total number", value = num, subtitle = paste(nrow(mm, "state")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)