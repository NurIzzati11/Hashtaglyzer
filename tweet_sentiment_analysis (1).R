#libraries
#NOTE:ALWAYS INSTALL THESE PACKAGES BEFORE CALLING THEIR LIBRARIES!!!
#Twitter API
library(rtweet)

#text
library(tidytext)
library(textdata)

#visualization
library(plotly)
library(ggwordcloud)
library(wordcloud)

#core
library(tidyverse)
library(tidyquant)

#--------------------
#Store API keys
api_key <- "6iNm7WhexYz3KDmtb22GT5krO"
api_secret_key <- "IxyRlTwxE5WSH6ajk95LlILoW0u5svtoIi6DOwAxOyh0eeFsLc"

#authenticate via web browser
token <- create_token(
  app = "Hastagslyzer",
  consumer_key = api_key,
  consumer_secret = api_secret_key
)

#view token
token

#---------------------
  

# search tweets
tweets <- rtweet::search_tweets(
  q = "#billgates" ,  # search hashtag
  n = 200, # number of results
  lang = "en", # language
  include_rts = FALSE # does not include rt
)

# results for first 5
tweets %>% glimpse()

# user info
tweets %>% slice(1:5) %>% select(screen_name, location, description)

# tweet info
tweets %>% slice(1:5) %>% select(text, url)

# hashtags info
tweets %>% slice(1:5) %>% select(hashtags) %>% unnest_wider(hashtags)

# url's in the tweet
tweets %>% slice(1:5) %>% select(urls_expanded_url) %>% unnest(urls_expanded_url)

#Stream tweets
#realtime twitter action

rt <- stream_tweets(timeout = 5)
rt %>% glimpse


# tidytext 
tweets_sentiment <- tweets

# tidy the data
tweets_tokenized_tbl <- tweets_sentiment %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text)

tweets_tokenized_tbl %>% count(word, sort = TRUE)

# sentiment analysis
get_sentiments(lexicon = "bing") #assign positive or negative features
get_sentiments(lexicon = "afinn") #assign polarity

# joining sentiment with text
sentiment_bing_tbl <- tweets_tokenized_tbl %>%
  inner_join(get_sentiments("bing"))

# measuring sentiment (overall)
sentiment_bing_tbl %>% count(sentiment)

# sentiment by user
sentiment_by_row_id_tbl <- sentiment_bing_tbl %>%
  select(-word) %>%
  count(rowid, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  mutate(sentiment = positive - negative) %>%
  left_join(
    tweets_sentiment %>% select(screen_name, text) %>% rowid_to_column()
  )

# polarity visualization
label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id_tbl %>%
  mutate(text_formatted = str_glue("ROW ID: {rowid}
                                   Screen Name: {screen_name}
                                   Text:
                                   {label_wrap(text)}"))

# ggplot of sentiment polarity
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
       y = "Sentiment")

ggplotly(g, tooltip = "text") %>%
  layout(
    xaxis = list(
      rangesSlider = list(type = "date")
    )
  )

# word cloud and bar graph
sentiment_by_word_tbl <- sentiment_bing_tbl %>%
  count(word, sentiment, sort = TRUE)

# word cloud of sentiment word frequency for negative and positive 
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

# bar graph of unique word counts found in tweets
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

#--------------------------------------------------------


