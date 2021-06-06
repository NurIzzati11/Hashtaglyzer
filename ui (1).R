library(shiny)

#Define UI

ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Hashtaglyzer"),
                headerPanel("Twitter Analytics - Sentiment Analysis"),

  # Getting User Inputs
sidebarLayout(  
sidebarPanel(
    
    wellPanel(
      textInput("entity1", "Enter the hashtag: ","#billgates"),
      HTML
      ("<div style='font-size: 10px;font-weight: bold'> Feel free to replace above text with your own hashtag starting with '#' </div>")
    )  ,
    wellPanel(
      sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=10,max=300,value=30,step=1), # The max can, of course, be increased
      actionButton(inputId='actb',icon =icon("twitter"), label="Search")
    )
  ),
  
mainPanel("Main panel",
          tabsetPanel(type = "tabs",
                      tabPanel("Unique word", plotOutput("unique_word")),
                      tabPanel("Word Cloud", plotOutput("wordcloud")),
                      tabPanel("Sentiment Polarity", plotOutput("sentiment_polarity"))
          )
           
)
    
  )
)

