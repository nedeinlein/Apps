library(shiny)
library(dplyr)
library(markdown)
suppressMessages(library(shiny))

ui <-navbarPage("National Beers 101",
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              #State Select select
                              selectInput("state", label = h3("State"), 
                                          choices = list("ALL","AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN","MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA","WA", "WV", "WI", "WY"), 
                                          selected = 1),
                              
                              #Chart Type select
                              selectInput("chart", label = h3("Chart Type"), 
                                          choices = list("Histogram" = "H", "Boxplot" = "B", "Scatterplot" = "S"), 
                                          selected = 1),
                              
                              
                              #ABV vs IBU selector 
                              selectInput("select", label = h3("Alcohol Content or Bitterness"), 
                                          choices = list("ABV", "IBU"), 
                                          selected = 1),
                              
                              radioButtons("reg", label = h3("Regression"),
                                           choices = list("Regression On" = 1, "Regression Off" = 2), 
                                           selected = 1),
                              
                              radioButtons("bt", label = h3("Beer Type"),
                                           choices = list("On" = 1, "Off" = 2), 
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
                                              includeMarkdown("https://raw.githubusercontent.com/nedeinlein/nedeinlein.github.io/main/About.rmd")
                                       ),
                                       column(3,
                                              img(class="img-polaroid",height="200%", width = "200%",
                                                  src=paste0("https://upload.wikimedia.org/wikipedia/commons/thumb/0/0a/MaryRose-wooden_tankard1.JPG/800px-MaryRose-wooden_tankard1.JPG")),
                                              tags$small(
                                                "Source: the Mary Rose Trust "
                                              )
                                       )
                                     )
                            )
                 )
  )


server <- function(input, output, session) {
  df <- read.csv('https://raw.githubusercontent.com/nedeinlein/Apps/main/Beer%20Data%20for%20Shiny%20app.csv')
  df$Beerclass <- as.factor(df$Beerclass)
  output$distPlot <- renderPlot({
    
    #state filter    
    state <- input$state
    
    if(state == "ALL")
    {
      data <- df
    }
    if(state != "ALL")
    {
      data <- df %<% filter(abbr = state)
    }
    #beer type color filter
    if(input$bt == 1)
    {
      u <- data$Beerclass
      v <- data$Beerclass_Colour
    }
    if(input$bt == 2)
    {
      u <- data$Beerclass
      v <-"#75AADB"
    }
    
    #method select
    if(input$select == "ABV")
    {
      w <- "Alcohol By Volume"
      x <- "International Bitterness Unit"
      y <- data$ABV
      z <- data$IBU
    }
    if(input$select == "IBU")
    {
      w <- "International Bitterness Unit"
      x <- "Alcohol By Volume"
      y <- data$IBU
      z <- data$ABV
    }
    
    #chart selection
    if(input$chart == "H")
    {
      hist(y, col = "#75AADB", border = "white",
           xlab = w,
           main = "Histogram")
    }
    if(input$chart == "B")
    {
      boxplot(y, col = "#75AADB", border = "black",
              ylab = w,
              main = "Boxplot")
    }
    if(input$chart == "S")
    {
      #regression selected
      if(input$reg == 1)
      {
        #beer class selected
        if(input$bt == 1)
        {
          v <- data$Beerclass_Colour
          plot(y, z, xlab = w, ylab = x, main = "Scatterplot ABV vs IBU", col = v)
          legend(x="bottomright", legend = levels(data$Beerclass), col=c("red","blue","darkgreen"), pch=1)
          abline(lm(z~y), col = "red")
        }
        #beer class NOT selected
        if(input$bt == 2)
        {
          plot(y, z, xlab = w, ylab = x, main = "Scatterplot ABV vs IBU", col = "#75AADB")
          abline(lm(z~y), col = "red")
        }
      }
      #regression NOT selected
      if(input$reg == 2)
      {
        #beer class selected
        if(input$bt == 1)
        {
          v <- data$Beerclass_Colour
          plot(y, z, xlab = w, ylab = x, main = "Scatterplot ABV vs IBU", col = v)
          legend(x="bottomright", legend = levels(data$Beerclass), col=c("red","blue","darkgreen"), pch=1)
        }
        #beer class NOT selected
        if(input$bt == 2)
        {
          plot(y, z, xlab = w, ylab = x, main = "Scatterplot ABV vs IBU", col = v)
        }
      }
    }
  })
  
  #summary Output
  output$summary <- renderPrint({
    summary(df)
  })
  
  #table Output
  output$table <- DT::renderDataTable({
    DT::datatable(df)
  })
}
shinyApp(ui, server, options = list(height = 1080, width = 1080))
