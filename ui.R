library(reactable)

ui <- fluidPage(headerPanel(" Hashtaglyzer - Twitter Sentiment Analysis"),
                
                # Getting User Inputs
                
                sidebarPanel(
                  tags$head(
                    tags$style("body {background-color: white; }"),
                    tags$style("label{font-family: BentonSans Book;}"),
                    tags$style('h1 {color:#0046FF;}'),
                    tags$style('body {color:black;}')),
                  textInput("searchTerm", "Enter hashtag to search", "#"),
                  sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=10,max=800,value=500, step = 30),
                  actionButton("get_data","Explore!")
                  
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("About",HTML("<div><h3>What is Hashtaglyzer?</h3></div>"), 
                             strong("About"),p("Hashtaglyzer was developed with the goal to see sentiments of
                                                twitter users based on the hashtags they used. Be it products or popular issue,
                                                this application will show your the twitter users' sentiment on said topic!"
                                               ,style = "font-family: 'times'; font-si16pt"), 
                             br(),
                             p("Here's the link to our github repository:"
                               ,style = "font-family: 'times'; font-si16pt"), uiOutput("github"),
                             br(),
                             strong("How to use it?"),p("1. Enter your hashtags along with the symbol '#'."
                                                        ,style = "font-family: 'times'; font-si16pt"),
                                                      p("2. Enter how many tweets you want to analyze using the slider."
                                                        ,style = "font-family: 'times'; font-si16pt"),
                                                      p("3. Press the button 'Explore!' to see your results."
                                                        ,style = "font-family: 'times'; font-si16pt"),
                                                      p(strong("IMPORTANT:") ,"It might take some time for the results to load depending on how many tweets you choose to analyze!"
                                                        ,style = "font-family: 'times'; font-si16pt"),
                             br(),
                             strong("The Tabs"),p(code("Raw Tweets:")," - this tab shows you a list of tweets associated with the hashtag you entered.
                                                You can sort the table according to the date and time, username, tweet,likes, retweets, and URLs."
                                                ,style = "font-family: 'times'; font-si16pt"),
                                                p(code("Wordcloud:")," - this tab shows you a wordcloud of most frequent words associated with the hashtag your entered."
                                                ,style = "font-family: 'times'; font-si16pt"),
                                                p(code("Sentiment Pie Chart:")," - this tab shows you pie chart of sentiment scores based on words in the tweets."
                                                ,style = "font-family: 'times'; font-si16pt"),
                                                p(code("Sentiment Histogram:")," - this tab shows you three histograms of positive scores, negative scores and overall scores."
                                                ,style = "font-family: 'times'; font-si16pt"),
                             br(),
                             strong("How does the score work?"), p("The scores of the tweets are based on a list of positive and negative words based on Liu 
                                                                 and Hu opinion lexicon. Each words has certain scores, ranging from 0 to 3, depending on its category.
                                                                 The dataset can be obtained from this link:"
                                                                 ,style = "font-family: 'times'; font-si16pt"),uiOutput("tab"),
                             br(),
                             p(strong("Calculations for Pie chart"), br(), "Each word category has a score from 0-3 for both positive and negative sentiments. For example, you want to see the score for positive sentiment.
                             Take the number of tweets of each positive score and multiply it with the score number. Then, sum all of the scores together to get the end result. This is the same for calculating the negative sentiments too.
                             Here is the formula:"
                             ,style = "font-family: 'times'; font-si16pt"),img(src = "formula.png"),
                             
                             br(),br(),
                             p(strong("Reading the Histograms"), br(), "For positive and negative sentiment histograms, it shows you the number of tweets based on its sentiment score. The overall histogram shows the overall score for the hashtag. "
                             ,br(),br(), code("Positive histogram:"),"- The higher the frequency of higher scores, the more positive the sentiments of the hashtag"
                             ,br(),br(), code("Negative histogram:"),"- The higher the frequency of higher scores, the more negative the sentiments of the hashtag"
                             ,br(),br(), code("Overall histogram:"),"- If the frequency of tweets is more towards the right end, the sentiment is deemed positive.
                             If the frequency of tweets is more towards the left end, the sentiment is deemed negative. If most of the frequency of tweets are at score 0, it means the sentiment is neutral."
                             ,style = "font-family: 'times'; font-si16pt"),
                             
                    ),
                    tabPanel("Raw Tweets",HTML("<div><h3>List of raw tweets with the hashtag</h3></div>"),reactableOutput("tweet_table")
                    ),
                    tabPanel("Word Cloud",HTML("<div><h3>Word cloud for most frequent words</h3></div>"),plotOutput("word")
                    ),
                    tabPanel("Sentiment Pie Chart",HTML("<div><h3>Sentiment Analysis Score in Pie Chart</h3></div>"), plotOutput("piechart"),
                    ),
                    tabPanel("Sentiment Histogram",HTML( "<div><h3> Sentiment Analysis Score In Histogram </h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")
                    ),
                  )#end of tabset panel
                )#end of main panel
)



