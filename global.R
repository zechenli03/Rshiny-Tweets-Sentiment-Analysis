# title: "IE6600 - Group 8 Project"
# author: "Zechen Li"


# Installing package if not already installed 
EnsurePackage<-function(x)
{x <- as.character(x)
if (!require(x,character.only=TRUE))
{
  install.packages(pkgs=x)
  require(x,character.only=TRUE)
}
}

#Identifying packages required  
PrepareTwitter<-function()
{
  EnsurePackage("dplyr")
  EnsurePackage("rtweet")
  EnsurePackage("httpuv")
  EnsurePackage("slam")
  EnsurePackage("wordcloud2")
  EnsurePackage("shiny")
  EnsurePackage("DT")
  EnsurePackage("stringr")
  EnsurePackage("shinythemes")
  EnsurePackage("tm")
  EnsurePackage("qdapRegex")
  EnsurePackage("rjson")
  EnsurePackage("jsonlite")
  EnsurePackage("leaflet")
  EnsurePackage("gganimate")
  EnsurePackage("lubridate")
  EnsurePackage("maps")
  EnsurePackage("ggthemes")
  EnsurePackage("ggdark")
  EnsurePackage("plotly")
  EnsurePackage("tibble")
  EnsurePackage("lubridate")
  EnsurePackage("gapminder")
  EnsurePackage("gifski")
  EnsurePackage("plotrix")
  EnsurePackage("reshape")
}

#devtools::install_github("lchiffon/wordcloud2")
#devtools::install_github("dgrtwo/gganimate")
library(dplyr)
library(tm)
library(ggdark)
library(rtweet) 
library(httpuv)
library(slam)
library(stringr)       # Removing characters
library(qdapRegex)     # Removing URLs 
library(shiny)
library(DT)
library(shinythemes)
library(wordcloud2)
library(rjson)
library(jsonlite)
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)
library(plotly)
library(tibble)
library(lubridate)
library(gapminder)
library(gifski)
library(plotrix)
library(reshape)

#Define key and secret 
consumer_key <- '3HtfVZ3HIv3oGRkbOwREJ4d5Y'
consumer_secret <- 'llNEksiO3S1aCzZAtBxeXnTBi2yegpQ92gLmw9zJWAqpog7FPo'
Access_token <-'739303876055072768-JMXifS6L1nMUWVDaf7N1sdsROVmlcf5'
Access_tokensecret <- 'CkF4TqAkXrZS71zrikSKTHCxs6Aph73n67zneFwf5JsYI'

#Create a token to connect to Twitter's API using your key and secret
token <- create_token(app="RyanDV", consumer_key, consumer_secret, 
                      Access_token, Access_tokensecret,set_renv = TRUE)


# Clean the tweets
TweetClean<-function(tweets)
{
  text <- 
    str_c(tweets$text, collapse = "") %>%
    tolower() %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    removeWords(stopwords("SMART")) %>%   # Remove common words (a, the, it etc.)
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(c("amp"))                   # Final cleanup of other small changes
  #removal of emoticons
  text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub="")) #If you wish to print emoticons just comment this line
  text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", text)
  return (text)
}

wordclouds2 <- function(tweets_clean)
{
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(tweets_clean)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
  return (textCorpus)
}

toptrends <- function(location)
{
  trend <- get_trends(location)
  trends <- trend %>%
    arrange(desc(tweet_volume)) %>%
    mutate(id = row_number())%>%
    select("Id" = id, "Trend" = trend, "Tweet Volume" = tweet_volume)
  return (trends[1:10,])
}

# create new df with just the tweet texts & usernames
world_map_plot <- function(tweet_geo_data)
{
  # plot points on top of a leaflet basemap
  
  site_locations_base <- leaflet(tweet_geo_data) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addCircleMarkers(lng = ~long, lat = ~lat, popup = ~tweet_text,
                     radius = 4, stroke = FALSE,color = "red",
                     fillOpacity = 0.6)
  
  return (site_locations_base)
}

animate_plot <- function(tweet_geo_data){
  
  tweet_geo_data$date_time <-format(tweet_geo_data$date_time, tz="America/Los_Angeles",usetz=TRUE)
  tweet_geo_data$date_time <- as.POSIXct(tweet_geo_data$date_time)
  
  world <- ggplot() +
    borders("world", colour = "gray35", fill = "gray80") +
    theme_map() 
  
  map <- world +
    geom_point(aes(x = long, 
                   y = lat, 
                   size = followers_count),
               data = tweet_geo_data, 
               colour = '#CC0000', alpha = .6) +
    scale_size(range = c(1,8),
               breaks = c(100, 500, 1000, 3000, 6000)) +
    labs(size = 'Followers',
         title = 'Time: {closest_state} PDT') +
    dark_theme_gray() +
    theme(
      legend.background = element_blank(),
      legend.position = c(0.05, 0.23),
      legend.key = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    transition_states(date_time, 0, 1, wrap = F) +
    shadow_mark()
  return(map)
}

TweetFrame<-function(tweets1)
{
  #removal of emoticons
  tweets1$text <- sapply(tweets1$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #If you wish to print emoticons just comment this line
  tweets1$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweets1$text)
  return (tweets1$text)
}

#"positive_words.txt" file
pos.words = scan('www/positive_words.txt', what='character', comment.char=';') 
#"negative_words.txt" file
neg.words = scan('www/negative_words.txt', what='character', comment.char=';')

wordDatabase<-function()
{
  pos.words<<-c(pos.words)
  neg.words<<-c(neg.words)
}

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    sentence = gsub('\n','',sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}


sentimentAnalyser<-function(result)
{
  #Creating a copy of result data frame
  test1=result[[1]]
  test2=result[[2]]
  test3=result[[3]]
  
  #Creating three different data frames for Score, Positive and Negative
  #Removing text column from data frame
  test1$text=NULL
  test2$text=NULL
  test3$text=NULL
  #Storing the first row(Containing the sentiment scores) in variable q
  q1=test1[1,]
  q2=test2[1,]
  q3=test3[1,]
  qq1=melt(q1, var='Score')
  qq2=melt(q2, var='Positive')
  qq3=melt(q3, var='Negative') 
  qq1['Score'] = NULL
  qq2['Positive'] = NULL
  qq3['Negative'] = NULL
  #Creating data frame
  table1 = data.frame(Text=result[[1]]$text, Score=qq1)
  table2 = data.frame(Text=result[[2]]$text, Score=qq2)
  table3 = data.frame(Text=result[[3]]$text, Score=qq3)
  
  #Merging three data frames into one
  table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
  
  return(table_final)
}

percentage<-function(table_final)
{
  #Positive Percentage
  
  #Renaming
  posSc=table_final$Positive
  negSc=table_final$Negative
  
  #Adding column
  table_final$PosPercent = posSc/ (posSc+negSc)
  
  #Replacing Nan with zero
  pp = table_final$PosPercent
  pp[is.nan(pp)] <- 0
  table_final$PosPercent = pp*100
  
  #Negative Percentage
  
  #Adding column
  table_final$NegPercent = negSc/ (posSc+negSc)
  
  #Replacing Nan with zero
  nn = table_final$NegPercent
  nn[is.nan(nn)] <- 0
  table_final$NegPercent = nn*100
  return(table_final)
}

wordDatabase()