library(reactable)

ui <- fluidPage(headerPanel(" Hashtaglyzer - Twitter Sentiment Analysis"),
                
                # Getting User Inputs
                
                sidebarPanel(
                  tags$head(
                    tags$style("body {background-color: black; }"),
                    tags$style("label{font-family: BentonSans Book;}"),
                    tags$style('h1 {color:#00acee;}'),
                    tags$style('body {color:#00acee;}')),
                  textInput("searchTerm", "Enter hashtag to search", "#"),
                  sliderInput("maxTweets","Number of recent tweets to use for analysis (p.s: more tweets will take longer to load):",min=10,max=800,value=500, step = 30),
                  actionButton("get_data","Explore!")
                  
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Raw Tweets",HTML("<div><h3>List of raw tweets with the hashtag</h3></div>"),reactableOutput("tweet_table")
                    ),
                    tabPanel("Word Cloud",HTML("<div><h3>Word cloud for most frequent words</h3></div>"),plotOutput("word")
                    ),
                    tabPanel("Sentiment Pie Chart",HTML("<div><h3>Sentiment Analysis Score in Pie Chart</h3></div>"), plotOutput("piechart"),textOutput("pieExp"),img(src = "formula.png")
                    ),
                    tabPanel("Sentiment Histogram",HTML( "<div><h3> Sentiment Analysis Score In Histogram </h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore"),uiOutput("histogramExp")
                    ),
                    tabPanel("Source", HTML("<div><h2>Sentiment score source</h2></div>"),uiOutput("tab")
                    ),
                  )#end of tabset panel
                )#end of main panel
)



