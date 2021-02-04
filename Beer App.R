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
                  choices = list("ALL" = "ALL", "AK" = "AK", "AL" = "AL", "AR" = "AR", "AZ" = "AZ", "CA" = "CA", 'CO' = 'CO', 'CT' = 'CT', 'DE' = 'DE'), 
                  selected = 1),
      
      #Chart Type select
      selectInput("chart", label = h3("Chart Type"), 
                  choices = list("Histogram" = "H", "Boxplot" = "B", "Scatterplot (ABV Only)" = "S"), 
                  selected = 1),
      
      #ABV vs IBU selector 
      selectInput("select", label = h3("Alcohol Content or Bitterness"), 
                  choices = list("ABV" = "ABV", "IBU" = "IBU"), 
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
    data <- df
    x <- data$abv
    
    if(input$chart == "H")
    {
      if(input$select == "ABV")
      {
        x    <- data$ABV
        
        hist(x, col = "#75AADB", border = "white",
             xlab = "Alcohol By Volume",
             main = "Histogram of ABV")
      }
      if(input$select == "IBU")
      {
        x    <- data$IBU
        
        hist(x, col = "#75AADB", border = "white",
             xlab = "International Bitterness Units",
             main = "Histogram US IBU")
      }
    }
    if(input$chart == "B")
    {
      if(input$select == "ABV")
      {
        x    <- data$ABV
        
        boxplot(x, col = "#75AADB", border = "Black",
                ylab = "Alcohol By Volume",
                main = "Boxplot of ABV")
        
      }
      if(input$select == "IBU")
      {
        x    <- data$IBU
        
        boxplot(x, col = "#75AADB", border = "Black",
                ylab = "International Bitterness Units",
                main = "Boxplot US IBU")
      }
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
