library(shiny)
library(ggplot2)
library(shinythemes)

server <- function(input, output) {
  
  output$unique_word <- renderPlot(
    sentiment_bing_tbl %>%
    count(word, sort = TRUE) %>%
      top_n(5) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(x = word, y = n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip() +
      theme_light() +
      labs(x = "Count",
           y = "Unique words",
           title = "Unique word counts found in tweets")
  )
  
  output$wordcloud <- renderPlot(
    sentiment_by_word_tbl %>%
      slice(1:100) %>%
      mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
      ggplot(aes(label = word, color = sentiment, size = n)) +
      geom_text_wordcloud_area() +
      facet_wrap(~ sentiment, ncol = 2) +
      theme_tq() + 
      scale_color_tq() +
      scale_size_area(max_size = 16) +
      labs(title = "Sentiment Word Frequency") +
      theme_tq()
  )
  
  output$sentiment_polarity <- renderPlot(
    g <- data_formatted %>%
      ggplot(aes(rowid, sentiment)) +
      geom_line(color = "#2c3e50", alpha = 0.5) +
      geom_point(aes(text = text_formatted), color = "#3c3e50") +
      geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
      geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
      geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
      geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") + 
      labs(title = "Sentiment Polarity", 
           x = "Twitter User", 
           y = "Sentiment"),
    
    ggplotly(g, tooltip = "text") %>%
      layout(
        xaxis = list(
          rangesSlider = list(type = "date"))
        )
      )
  
  
  
  
}

