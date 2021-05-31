#TWITTER API

#Twitter API
library(rtweet)

#Core
install.packages("tidyverse")
library(tidyverse)

#account setup
token <- create_token{
  
  app = "Hashtaglyzer"
  consumer_key = "6iNm7WhexYz3KDmtb22GT5krO"
  consumer_secret = "IxyRlTwxE5WSH6ajk95LlILoW0u5svtoIi6DOwAxOyh0eeFsLc"

}

#search tweets
tweets <- rtweet::search_tweets(
  q = "#covid19",  #search
  n = 1000, #number of results
  lang = "en", #language
  include_rts = FALSE #didnt include rt
)

#store the tweets into rds
write_csv(tweets, 'tweets.csv')

#results for first 5
tweets %>% glimpse()

#user info
tweets %>% slice(1:5) %>% select(screen_name, location, description)

#tweet info
tweets %>% slice(1:5) %>% select(text, url)

#hashtags info
tweets %>% slice(1:5) %>% select(hashtags) %>% unnest_wider(hashtags)

#url's in the tweet
tweets %>% slice(1:5) %>% select(urls_expanded_url) %>% unnest(urls_expanded_url)

##STREAM TWEETS---
#realtime twitter action

rt <- stream_tweets(timeout = 5)
rt %>% glimpse

#-------------------------------
#libraries ---
#text
library(tidytext)
library(textdata)

#visualization
library(plotly)
#install.packages("ggwordcloud")
library(ggwordcloud)

#core
library(tidyverse)
library(tidyquant)

#tidytext ---

tweets_sentiment <- tweets

#tidy the data
tweets_tokenized_tbl <- tweets_sentiment %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text)

#view
tweets_tokenized_tbl

tweets_tokenized_tbl %>% count(word, sort = TRUE)

#sentiment analysis ---
get_sentiments(lexicon = "bing") #assign positive or negative features
get_sentiments(lexicon = "afinn") #assign polarity

#joining sentiment with text
sentiment_bing_tbl <- tweets_tokenized_tbl %>%
  inner_join(get_sentiments("bing"))

#measuring sentiment (overall)
sentiment_bing_tbl %>% count(sentiment)

#sentiment by user
sentiment_by_row_id_tbl <- sentiment_bing_tbl %>%
  select(-word) %>%
  count(rowid, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
  mutate(sentiment = positive - negative) %>%
  left_join(
    tweets_sentiment %>% select(screen_name, text) %>% rowid_to_column()
  )

sentiment_by_row_id_tbl

#polarity visualization---

label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id_tbl %>%
  mutate(text_formatted = str_glue("ROW ID: {rowid}
                                   Screen Name: {screen_name}
                                   Text:
                                   {label_wrap(text)}"))

g <- data_formatted %>%
  ggplot(aes(rowid, sentiment)) +
  geom_line(color = "#2c3e50", alpha = 0.5) +
  geom_point(aes(text = text_formatted), color = "#3c3e50") +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
  geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
  geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
  geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
  theme_tq() +
  labs(title = "Sentiment Polarity", x = "Twitter User", y = "Sentiment")

ggplotly(g, tooltip = "text") %>%
  layout(
    xaxis = list(
      rangesSlider = list(type = "date")
    )
  )


#wordcloud ---
install.packages("wordcloud")
library(wordcloud)
sentiment_by_word_tbl <- sentiment_bing_tbl %>%
  count(word, sentiment, sort = TRUE)


sentiment_by_word_tbl %>%
  slice(1:100) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  ggplot(aes(label = word, color = sentiment, size = n)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ sentiment, ncol = 2) +
  theme_tq() + 
  scale_color_tq() +
  scale_size_area(max_size = 16) +
  labs(title = "Sentiment Word Frequency")

#top words tweets for hashtags
sentiment_bing_tbl %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in tweets")

