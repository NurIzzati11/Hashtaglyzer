library(shiny)

#Set working directory
getwd()
#t")

#---------------------
#STEP 1: Load packages

## install httpuv if not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

#install rtweet from CRAN
install.packages("rtweet")

#load rtweet package
library(rtweet)
library(dplyr)
library(tidyr)
install.packages("textdata")
library(tidytext) 
library(textdata) 
library(ggplot2)

#--------------------
#STEP 2: Store API keys
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
#STEP 3: Extract data
# n=number of tweets contain the #
word1 <- search_tweets(
  "#Apple", n=50, include_rts = FALSE)

word2 <- search_tweets(
  "#Samsung", n=50, include_rts = FALSE)

#---------------------
#STEP 4: process into tidy text or corpus object
#tidy text takes only the screen_name(username) & the text of tweet
#tidy text word 1
tweets.Word1 = word1 %>% select(screen_name, text)
#tidy text word 2
tweets.Word2 = word2 %>% select(screen_name, text)

#---------------------
#STEP 5: pre-processing

#5.1.1 Remove http elements manually word 1
tweets.Word1$stripped_text1 <- gsub("http\\S+", "", tweets.Word1$text)

#5.2.1 Remove punctuation, and add ID for each tweet word 1
tweets.Word1_stem <- tweets.Word1 %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1) #to convert to lowercase

#5.3.1 Remove stop words from your list of words word 1
cleaned_tweets.Word1 <- tweets.Word1_stem %>%
  anti_join(stop_words) #anti_join: to remove the stop words from the tweets that were extracted
head(cleaned_tweets.Word1)
head(tweets.Word1$text)


#5.1.2 Remove http elements manually word 2
tweets.Word2$stripped_text2 <- gsub("http\\S+", "", tweets.Word2$text)

#5.2.2 Remove punctuation, and add ID for each tweet word 2
tweets.Word2_stem <- tweets.Word2 %>%
  select(stripped_text2) %>%
  unnest_tokens(word, stripped_text2)

head(tweets.Word2_stem)

#5.3.2 Remove stop words from your list of words word 2
cleaned_tweets.Word2 <- tweets.Word2_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Word2)

#Store data in csv file -> use in Shiny to preview most used words for the hashtag
write.csv(cleaned_tweets.Word1, 'Word1_tweets.csv')
write.csv(cleaned_tweets.Word2, 'Word2_tweets.csv')

#---------------------
#like this, nanti implementkan guna shiny
#STEP 6: plotting Top 10 words in 
#top 10 Word1 tweets
cleaned_tweets.Word1 %>%
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
       title = "Unique word counts found in #word1 tweets")

#top 10 Word2 tweets
cleaned_tweets.Word2 %>%
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
       title = "Unique word counts found in #word2 tweets")

#----------------------------
#STEP 7: sentiment analysis 

#======NO NEED=========NO NEED================
#use bing lexicon and tidytext function

get_sentiments("bing") %>% filter(sentiment=="positive")
get_sentiments("bing") %>% filter(sentiment=="negative")

get_sentiments("afinn") %>% filter(value=="3")
get_sentiments("afinn") %>% filter(value=="-3")

#======NO NEED=========NO NEED================

## bing sentiment analysis, returns tibble
# word1
bing_word1 = cleaned_tweets.Word1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# comparing positive and negative words in word 1
bing_word1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#WORD1'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

# word2
bing_word2 = cleaned_tweets.Word2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# comparing positive and negative words in word 1
bing_word2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#WORD2'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#----------------------------
#step 8: sentiment score for each tweet
sentiment_bing = function(twt){
  #1; perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>%
    mutate(
      #remove http elements manually
      stripped_text = gsub("htpp\\S+","", text)
    ) %>%
    unnest_tokens(word,stripped_text) %>%
    anti_join(stop_words) %>% #remove stop words
    inner_join(get_sentiments("bing")) %>% #merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
    
    ##create a column "score", that assigns a -1 one to all negative words, and 1 to positive words
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive' ~ n*(1))
    )
  
  ##calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #if there are no words, score=0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  
  ##to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1: no words at all, zero=no
    nrow(twt_tbl)>0~"Type 2", #Type 2: zero means sum of words=0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
  
}

#apply the function
#lapply = returns a list of  all sentiment scores, types, tables of the tweets
word1_sent = lapply(word1$text,function(x){sentiment_bing(x)})
word1_sent # no need

word2_sent = lapply(word2$text,function(x){sentiment_bing(x)})
word2_sent # no need

library(purrr)
#library(repurrrsive)

word_sentiment = bind_rows(
  tibble(
    word = '#word1',
    score = unlist(map(word1_sent, "score")),
    type = unlist(map(word1_sent, "type"))
  ),
  tibble(
    word = '#word2',
    score = unlist(map(word2_sent, 'score')),
    type = unlist(map(word2_sent, 'type'))
  )
)

#side by side histogram, key summary statistics.
#still cant figure out on how to implement this on shiny app..
ggplot(word_sentiment, aes(x=score, fill = word)) + 
  geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~word) + theme_bw()

# Run the application 
shinyApp(ui = ui, server = server)
