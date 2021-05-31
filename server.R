library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$word_sentiment <- renderPlot(
    ggplot(word_sentiment, aes(x=score, fill = word)) + 
      geom_histogram(bins = 15, alpha = .6) +
      facet_grid(~word) + theme_bw()
  )
}
 

