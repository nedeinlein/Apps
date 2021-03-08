suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(dplyr))
suppressMessages(library(markdown))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyverse))
suppressMessages(library(plotly))

vardf <- read.csv("https://raw.githubusercontent.com/nedeinlein/Apps/main/Customer%20List.csv")

ui <-navbarPage("Revenue Tracker", theme = shinytheme("slate"),
                tabPanel("Revenue",
                         sidebarLayout(
                           sidebarPanel(
                             #CSV upload
                             fileInput("file", "Choose CSV File", accept = ".csv"),
                             checkboxInput("header", "Header", TRUE),
                             
                             #Select Year
                             selectInput("year", label = h3("Year"), 
                                         choices = c(sort(unique(vardf$Year))), 
                                         selected = 1),
                             
                             #Revenue Type 
                             selectInput("portal", label = h3("Portal"), 
                                         choices = list("All", "MOBI", "Austin"), 
                                         selected = 1),
                             
                             #Customer Code
                             selectInput("cust", label = h3("Customer"), 
                                         choices = c("All", sort(unique(vardf$WMS.Item.family))), 
                                         selected = 1),
                             #Revenue Type
                             selectInput("type", label = h3("Revenue Type"), 
                                         choices = c("All", sort(unique(vardf$Revenue.Type))), 
                                         selected = 1),
                             hr(),
                             fluidRow(column(3, verbatimTextOutput("revenue")))
                           ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             # Output: Histogram ----
                             plotOutput(outputId = "distPlot1")
                           )
                         )
                ),
                tabPanel("Throughput",
                         sidebarLayout(
                           sidebarPanel(
                             #CSV upload
                             fileInput("file2", "Choose CSV File", accept = ".csv"),
                             checkboxInput("header", "Header", TRUE),
                             
                             #Select Year
                             selectInput("year2", label = h3("Year"), 
                                         choices = c(sort(unique(vardf$Year))), 
                                         selected = 1),
                             
                             #Revenue Type 
                             selectInput("portal2", label = h3("Portal"), 
                                         choices = list("All", "MOBI", "Austin"), 
                                         selected = 1),
                             
                             #Customer Code
                             selectInput("cust2", label = h3("Customer"), 
                                         choices = c("All", sort(unique(vardf$WMS.Item.family))), 
                                         selected = 1),
                             #Revenue Type
                             selectInput("type2", label = h3("Transaction Type"), 
                                         choices = c("All", sort(unique(vardf$Transaction.Type
))), 
                                         selected = 1),
                             hr(),
                             fluidRow(column(3, verbatimTextOutput("throughput")))
                           ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             # Output: Histogram ----
                             plotOutput(outputId = "distPlot2")
                           )
                         )
                ),
                tabPanel("Summary",
                         verbatimTextOutput("summary")
                ),
                navbarMenu("More",
                           tabPanel("Table",
                                    DT::dataTableOutput("table")
                           ),
                           tabPanel("About",
                                    fluidRow(
                                      column(6,
                                             includeMarkdown("https://raw.githubusercontent.com/nedeinlein/Apps/main/Revenuetracker.About.rmd")
                                      ),
                                    )
                           )
                )
)


server <- function(input, output, session) {

  
  #Output revenue
  output$distPlot1 <- renderPlot({
    #reactive filters
    data <- reactive({
      file <-input$file
      df <- read.csv(file$datapath, header = input$header)
      rows <- (input$year == "All" | df$Year == input$year) &
        (input$portal == "All" | df$Revenue.Stream == input$portal) &
        (input$cust == "All" | df$WMS.Item.family == input$cust) &
        (input$type == "All" | df$Revenue.Type == input$type)
      df[rows,,drop = FALSE]  
    })
    req(data())
   data () %>% mutate(Month = fct_reorder(Month, Month.Number)) %>% ggplot(aes(x = Month, y = Rev.Type.Total, fill = Revenue.Type)) + geom_col() + ylab("Revenue")
  })
  
  #Output transaction
  output$distPlot2 <- renderPlot({
    data2 <- reactive({
      file2 <-input$file2
      df2 <- read.csv(file2$datapath, header = input$header)
      rows <- (input$year2 == "All" | df2$Year == input$year2) &
        (input$portal2 == "All" | df2$Revenue.Stream == input$portal2) &
        (input$cust2 == "All" | df2$WMS.Item.family == input$cust2) &
        (input$type2 == "All" | df2$Transaction.Type == input$type2)
      df2[rows,,drop = FALSE]  
    })
    req(data2())
    data2() %>% mutate(Month = fct_reorder(Month, Month.Number)) %>% ggplot(aes(x = Month, y = Transaction.Count, fill = Transaction.Type)) + geom_col() + ylab("Activity Count") + ggtitle("Transaction Count")
  })
  
  #summary Output
  output$summary <- renderPrint({
    summary(data())
  })
  
  #table Output
  output$table <- DT::renderDataTable({
    DT::datatable(data())
  })
}

shinyApp(ui, server)
