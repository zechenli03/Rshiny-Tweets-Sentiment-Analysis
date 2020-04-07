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
  
  tweets_map <- reactive({
    search_tweets(hashtag(), 
                  input$number, 
                  include_rts = FALSE)
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
      wordcloud2(color='random-light', 
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
               color='random-light', 
               backgroundColor="transparent",
               fontWeight = "bold")
  })
  
  trend_table <- reactive({ 
    req(input$location)
    trend_table <-
      toptrends(input$location) })
  
  output$trendtable <- 
    renderTable({trend_table()})
  
  tweet_geo_data <- reactive({
    geo_data <- lat_lng(tweets_map())
    geo_data <- geo_data %>% rename(long = lng)
    tweet_geo_data <- geo_data %>% 
      select(date_time = created_at,
             followers_count, 
             long, 
             lat,
             tweet_text = text) %>%
      na.omit()
  })
  
  output$map <- 
    renderLeaflet({world_map_plot(tweet_geo_data())})
  
  output$title <- renderText({
    x <- input$hashtag
    y <- nrow(tweet_geo_data())
    z <- input$number
    paste("Geographical Distribution of the Latest", z, "Tweets (",y,"Tweets with a Location) Containing Hashtag #", x, ":")
  })
  
  output$title2 <- renderText({
    x <- input$hashtag
    y <- nrow(tweet_geo_data())
    z <- input$number
    paste("Anamited Geographical Distribution of the Latest", z, "Tweets (",y,"Tweets with a Location) Containing Hashtag #", x, ":")
  })
  
  animated <- reactive({
    animate_plot(tweet_geo_data())
  })
  
  animate_map_plot <- reactive({
    p <- animate(animated(),width = 935, height = 600, end_pause = 10, fps=5)
    anim_save("animate.gif", p)
    
    list(src = "animate.gif",
         contentType = 'image/gif'
    )
  })
  
  output$animate_map <- renderImage({
    animate_map_plot()
  }, deleteFile = TRUE)
    
  tweetsTable <- reactive({
    tweetsTable<-TweetFrame(tweets1())
  })
  
  result <- reactive({
    result <- score.sentiment(
      tweetsTable(), 
      pos.words, 
      neg.words, 
      .progress='none')
  })
  
  table_final <- reactive({
    table_final <- sentimentAnalyser(result())
  })
  
  table_final_percentage <- reactive({
    table_final_percentage <- percentage(table_final())
    })
  
  output$tweetstable <- DT::renderDataTable({
    DT::datatable(
      table_final_percentage(), 
      options = list(dom = 'ltipr',
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'gray85', 'color': 'white'});",
                       "}")
                     )
    ) %>%
      formatStyle(c(" ", "Text", "Positive", "Negative", "Score", "PosPercent", "NegPercent"),  
                  color = 'white',
                  backgroundColor = 'black', 
                  fontWeight = 'bold',
                  borderColor = "#545454")
  })
  
  output$histPos<- renderPlot({ 
    a<- table_final()
    
    ggplot(a,
           aes(x = factor(Positive),
               alpha = 0.8,
               fill = factor(Positive))) +
      geom_bar() +
      geom_text(aes(label=..count..),
                stat='count',
                vjust = -0.5,
                size = 4) +
      labs(x = "Positive Score",
           y = "Count",
           title = "Histogram of Positive Sentiment") + 
      dark_theme_gray() + 
      guides(alpha = FALSE,
             fill=guide_legend(title="Scores")) +  
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none"
      )
  })
  
  output$histNeg<- renderPlot({
    a<- table_final()
    
    ggplot(a,
           aes(x = factor(Negative),
               alpha = 0.8,
               fill = factor(Negative))) +
      geom_bar() +
      geom_text(aes(label=..count..),
                stat='count',
                vjust = -0.5,
                size = 4) +
      labs(x = "Negative Score",
           y = "Count",
           title = "Histogram of Negative Sentiment") + 
      dark_theme_gray() + 
      guides(alpha = FALSE,
             fill=guide_legend(title="Scores")) +  
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none"
      )
  })
  
  output$histScore<- renderPlot({ 
    a<- table_final()
    
    ggplot(a,
           aes(x = factor(Score),
               alpha = 0.8,
               fill = factor(Score))) +
      geom_bar() +
      geom_text(aes(label=..count..),
                stat='count',
                vjust = -0.5,
                size = 4) +
      labs(x = "Overall Score",
           y = "Count",
           title = "Histogram of Overall Score Sentiment") + 
      dark_theme_gray() + 
      guides(alpha = FALSE,
             fill=guide_legend(title="Scores")) +  
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none"
      )
    })
  
  #Pie
  slices <- reactive ({ 
    t <- table_final()
    p <- t %>%
      filter(Score > 0)
    n <- t %>%
      filter(Score == 0)
    ne <- t %>%
      filter(Score < 0)
    a <- data.frame(
      Sentiment = c("Positive", "Neutral","Negative"),
      num = c(nrow(p),nrow(n),nrow(ne)))
    
    slices <- a %>%
      mutate(percentage = round(num/sum(num),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage)
  })
  
  output$piechart <- renderPlot({ 
    
    ggplot(data = slices(), 
           aes(x = 2, y = percentage, fill = Sentiment))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 200) +
      geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white")+
      labs(title = "Overall Sentiment Polarity Percentage",
           subtitle = "
           Overall Score > 0 means Positive
           Overall Score = 0 means Neutral
           Overall Score < 0 means Negative") +
      dark_theme_gray() + 
      scale_fill_brewer(palette = "Dark2") +  
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.background = element_blank(),
            legend.key = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      xlim(.2,2.5) 

  })
  
}
  





