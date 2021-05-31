library(shiny)

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Hashtaglyzer"),
  
  # Sidebar with a space to enter password
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "type", 
                  label = strong("Word"),
                  choices = c("Apple", "Samsung"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("word_sentiment")
    )
  )
)
