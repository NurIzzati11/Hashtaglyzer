library(shiny)
library(shinythemes)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)
library(stringr)
library(ROAuth)
library(RCurl)
library(ggplot2)
library(reshape)
library(tm)
library(wordcloud)
library(wordcloud2)
library(gridExtra)
library(syuzhet)
library(plotrix)
library(twitteR)
library(plyr)
library(reactable)


server <- function(input, output) {
  make_url_html <- function(url) {
    if(length(url) < 2) {
      if(!is.na(url)) {
        as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
      } else {
        ""
      }
    } else {
      paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
  }
  
  # For accessing the API, enter the key and token.
  api_key <- "DJt4xHpvDjlOImVIWmVBwjG0W"
  api_secret <- "cyPty7bSajiZ87ybDHbyRyf6QdVVKLhsc7ujfBKuExbwAoTD0D"
  access_token <- "4336372933-xhj7p1wOi7kjwtejIf5ZvVc0XpKqFMRPZ0F45PT" 
  access_token_secret <- "d8Q6b0rDupmziUNPoTrVvnRtT651u2IIgBteit1351VUm"
  
  # After setting up credentials, setup access using OAUTH protocol
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  # we assume that the positive and negative words. These words are available on the web to identify if a sentence is positive/negative.
  positive = scan('positive_words.txt', what='character', comment.char=';')
  negative = scan('negative_words.txt', what='character', comment.char=';')
  
  url <- a("Click here!", href="https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html")
  output$tab <- renderUI({
    tagList("The scores of the tweets are based on a list of positive and negative words based on Liu 
            and Hu opinion lexicon. Each words has certain scores, ranging from 0 to 3, depending on its category.
            .The dataset can be obtained from this link:", url)
  })
  
  # Display sentiment analysis in tabular format
  analyze_sentiments<-function(result)
  {
    # Let us create a copy first.
    test_positive=result[[2]]
    test_negative=result[[3]]
    
    # Create some temporary dfs for positive and negatives.
    test_positive$text=NULL
    test_negative$text=NULL
    
    # Create a copy and some temporary DFs for score.
    test_score=result[[1]]
    test_score$text=NULL
    
    # Storing the first row(Containing the sentiment scores)
    overall_row=test_score[1,]
    positive_row=test_positive[1,]
    negative_row=test_negative[1,]
    overall_melt=melt(overall_row, var='Score')
    positive_melt=melt(positive_row, var='Positive')
    negative_melt=melt(negative_row, var='Negative') 
    overall_melt['Score'] = NULL
    positive_melt['Positive'] = NULL
    negative_melt['Negative'] = NULL
    
    # Using this data, create a dataframe for positive, negative data frames.
    overall_df = data.frame(Text=result[[1]]$text, Score=overall_melt)
    positive_df = data.frame(Text=result[[2]]$text, Score=positive_melt)
    negative_df = data.frame(Text=result[[3]]$text, Score=negative_melt)
    
    # Merging three data frames into one
    final_df=data.frame(Text=overall_df$Text, Positive=positive_df$value, Negative=negative_df$value, Score=overall_df$value)
    
    return(final_df)
  }
  
  compute_percentage<-function(final_df)
  {
    # Use temporaries for local function use.
    pos_score=final_df$Positive
    neg_score=final_df$Negative
    
    final_df$PosPercent = pos_score/ (pos_score+neg_score)
    
    # Let us replace NAs with zero. This is because certain hashtags might not have tweets and we would need to 
    # replace them.
    positive_score = final_df$PosPercent
    positive_score[is.nan(positive_score)] <- 0
    final_df$PosPercent = positive_score*100
    
    # Calculating negative percentage.
    final_df$NegPercent = neg_score/ (pos_score+neg_score)
    
    # ditto as above. Let us replace NAs with zero. This is because certain hashtags might not have tweets and we would need to 
    # replace them.
    negative_score = final_df$NegPercent
    negative_score[is.nan(negative_score)] <- 0
    final_df$NegPercent = negative_score*100
    return(final_df)
  }
  
  positive_words<<-c(positive)
  negative_words<<-c(negative)
  
  tweet_list<-eventReactive(input$get_data,{tweet_list<-searchTwitter(input$searchTerm, n=input$maxTweets, lang="en") })
  
  search_tweets_and_return<-function(tweet_list)
  {
    # Let us transform this to a df.
    twitter_data<- do.call("rbind",lapply(tweet_list,as.data.frame))
    
    # While it is fun to show them, emoticons will be removed from the tweets for better view as well
    twitter_data$text <- sapply(twitter_data$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    
    # Some regex magic to remove the URLs: https://stackoverflow.com/questions/11331982/how-to-remove-any-url-within-a-string-in-python
    twitter_data$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", twitter_data$text)
    return (twitter_data$text)
  }
  
  tweets<-reactive({tweets<-search_tweets_and_return(tweet_list() )})
  
  # Calculating sentiment score
  compute_sentiment_score <- function(sentences, positive, negative, .progress='none')
  {
    list=lapply(sentences, function(sentence, positive, negative)
    {
      clean_sentence = gsub('[[:punct:]]',' ',sentence) 
      clean_sentence = gsub('[[:cntrl:]]','',sentence)
      clean_sentence = gsub('\\d+','',sentence) # Removes decimal numbers
      clean_sentence = gsub('\n','',sentence) # Removes new lines
      
      clean_sentence = tolower(clean_sentence)
      word_list = str_split(clean_sentence, '\\s+')
      words = unlist(word_list)
      pos_matches = match(words, positive)
      neg_matches = match(words, negative) 
      pos_matches = !is.na(pos_matches) # Add only positive words and no NA
      neg_matches = !is.na(neg_matches)
      positive_score=sum(pos_matches)
      negative_score = sum(neg_matches)
      score = sum(pos_matches) - sum(neg_matches)
      list1=c(score, positive_score, negative_score) # Append the scores to the list
      return (list1)
    }, positive, negative)
    score_new=lapply(list, `[[`, 1)
    positive_score_list=score=lapply(list, `[[`, 2)
    negative_score_list=score=lapply(list, `[[`, 3)
    
    scores_df = data.frame(score=score_new, text=sentences)
    positive_df = data.frame(Positive=positive_score_list, text=sentences)
    negative_df = data.frame(Negative=negative_score_list, text=sentences)
    
    list_df=list(scores_df, positive_df, negative_df)
    return(list_df)
  }
  
  result<-reactive({result<-compute_sentiment_score(tweets(), positive, negative, .progress='none')})
  
  final_df<-reactive({final_df<-analyze_sentiments(result())})
  
  # Utility to clean the word cloud data.
  clean_wordcloud_data<-function(text)
  {
    corpus_data <- VCorpus(VectorSource(text))
    
    # Cleanse data for word cloud by transforming the case, removing stop words, whitespaces etc., and return the data. 
    word_cloud_data <- tm_map(corpus_data, removePunctuation)
    word_cloud_data <- tm_map(word_cloud_data, content_transformer(tolower))
    word_cloud_data <- tm_map(word_cloud_data, removeWords, stopwords("english"))
    word_cloud_data <- tm_map(word_cloud_data, removeNumbers)
    word_cloud_data <- tm_map(word_cloud_data, stripWhitespace)
    return (word_cloud_data)
  }
  
  text_word<-reactive({text_word<-clean_wordcloud_data(tweets())})
  
  output$word <- renderPlot({ wordcloud(text_word(),random.order=F,max.words=80,col=rainbow(100), main="WordCloud",scale=c(4.5,1))})
  
  # Plot for positive and negative words along with the overall score.
  output$histPos<- renderPlot({ hist(final_df()$Positive, col=rainbow(10), main="Positive Sentiment", xlab = "Positive Score") })
  output$histNeg<- renderPlot({ hist(final_df()$Negative, col=rainbow(10), main="Negative Sentiment", xlab = "Negative Score") })
  output$histScore<- renderPlot({ hist(final_df()$Score, col=rainbow(10), main="Overall Score", xlab = "Overall Score") })	
  
  #exp for histogram
  output$histogramExp <- renderUI({
    tagList("Overall Score - Depending on the frequency of tweets against the score, the more positive the score, 
            the more positive is the sentiment and vice versa.")
  })
  
  #pie chart
  slices <- reactive ({slices <- c(sum(final_df()$Positive), sum(final_df()$Negative)) })
  test<- c("Postive", "Negative")
  labels<- reactive(paste0(test, " Score =", {labels <- c(sum(final_df()$Positive), sum(final_df()$Negative))},""))
  output$piechart <- renderPlot({ pie(slices(), labels = labels(), col=rainbow(length(labels())),explode=0.00, main="Sentiment Analysis (Pie Chart)") })
  
  #pie chart explanation
  output$pieExp <- renderText({ 
    "Formula for each category"
  })
  
  
  #showing raw tweets
  tweet_df <- eventReactive(input$get_data, {
    search_tweets2(input$searchTerm, n = input$maxTweets, lang="en", include_rts = FALSE)
  })

  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })

  output$tweet_table <- renderReactable({
    reactable::reactable(tweet_table_data(),
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200),
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc"),
                           User = colDef(defaultSortOrder = "asc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                           URLs = colDef(html = TRUE)
                         )
    )
  })
  
}