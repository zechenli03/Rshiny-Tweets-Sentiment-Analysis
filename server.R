# title: "IE6600 - Group 8 Project"
# author: "Zechen Li"

server <- function(input, output) {
  
  ## Wordcloud
  #Adding '#' before the hashtag
  hashtag <- reactive({
    req(input$hashtag)
    paste("#", input$hashtag)
  })
  
  #Using input hashtag to collect data from Twitter
  tweets <- reactive({
    req(input$language)
    if(input$language == "Null"){
      search_tweets(hashtag(), 
                    input$number, 
                    include_rts = FALSE)
    } else {
      search_tweets(hashtag(), 
                    input$number, 
                    include_rts = FALSE, #Don't collect retweets
                    lang=input$language) 
    }
  })
  
  tweets1 <- reactive({
    search_tweets(hashtag(), 
                  input$number, 
                  include_rts = FALSE, #Don't collect retweets
                  lang="en")
  })
  
  #Unlist hashtags
  hashtags_list <- reactive({
    tweets()$hashtags %>%
      unlist() %>%
      tolower()
  })
  
  #Make a frequency table of hashtags
  hashtags_table <- reactive({
    table(hashtags_list())
  })
  
  #Transform table to dataframe
  hashtags_df <- reactive({
    cbind.data.frame(tags = names(hashtags_table()),
                     count = as.integer(hashtags_table()))
  })
  
  #Sort the dataframe in decending order
  hashtags_df_sort <- reactive({
    hashtags_df() %>%
      arrange(desc(hashtags_df()$count))
  })
  
  # Create the wordcloud1 from the hashtag
  output$wordcloud1 <- renderWordcloud2({
    hashtags_df_sort() %>%
      filter(hashtags_df_sort()$tags != input$hashtag) %>%
      wordcloud2(size=1.4, 
                 color='random-light', 
                 backgroundColor="transparent",
                 fontWeight = "bold")
  })
  
  #Clean tweets(removal of emotions, urls)
  tweets_clean <- reactive({
    TweetClean(tweets1())
  })
  
  #Clean Wordcloud2
  textCorpus<-reactive({
    wordclouds2(tweets_clean())
  })
  
  # Create the wordcloud2 from the hashtag
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(textCorpus(),
               size=1.4, 
               color='random-light', 
               backgroundColor="transparent",
               fontWeight = "bold")
  })
  
  
  
}