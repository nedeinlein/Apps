library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Beer Data App"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #State Select select
      selectInput("state", label = h3("State"), 
                  choices = list("ALL","AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN","MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA","WA", "WV", "WI", "WY"), 
                  selected = 1),
      
      #Chart Type select
      selectInput("chart", label = h3("Chart Type"), 
                  choices = list("Histogram" = "H", "Boxplot" = "B", "Scatterplot (ABV Only)" = "S"), 
                  selected = 1),
      
      
      #ABV vs IBU selector 
      selectInput("select", label = h3("Alcohol Content or Bitterness"), 
                  choices = list("ABV", "IBU"), 
                  selected = 1),
      
      radioButtons("radio", label = h3("Regression"),
                   choices = list("Regression On" = 1, "Regression Off" = 2), 
                   selected = 1),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
# Define server logic required to draw a histogram ----
library(ggplot2)
library(dplyr)
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    df <- read.csv('https://raw.githubusercontent.com/nedeinlein/nedeinlein.github.io/main/Beer%20Data%20for%20Shiny%20app.csv')
    
    
    #state filter    
    state <- input$state
    
    if(state == "ALL")
    {
      data <- df
    }
    if(state != "ALL")
    {
      data <- df %>% filter(abbr == state)
    }
    #method select
    if(input$select == "ABV")
    {
      x <- data$ABV
      y <- "Alcohol By Volume"
    }
    if(input$select == "IBU")
    {
      x <- data$IBU
      y <- "International Bitterness Unit"
    }
    
    #chart selection
    if(input$chart == "H")
    {
      hist(x, col = "#75AADB", border = "white",
           xlab = y,
           main = "Histogram")
    }
    if(input$chart == "B")
    {
      boxplot(x, col = "#75AADB", border = "Black",
              ylab = y,
              main = "Boxplot")
    }
    if(input$chart == "S")
    {
      if(input$select == "ABV")
      {
        data %>% ggplot(aes(x=ABV, y = IBU)) + geom_point(color = "#75AADB") + ggtitle("Scatterplot of ABV vs IBU")
      }
    }
  })
  
}
shinyApp(ui, server)
