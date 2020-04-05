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

animate_map_plot <- function(tweet_geo_data){
  world_basemap <- ggplot() +
    borders("world", colour = "gray85", fill = "gray80") +
    theme_map() + dark_theme_gray()
  
  grouped_tweet_map <- world_basemap + geom_point(data = tweet_geo_data,
                                                  aes(long, lat),
                                                  color = "purple", alpha = .5) +
    transition_manual(date_time)+
    coord_fixed()
  
  animate(animate_map_plot, duration = 5, fps = 1, width = 1280, height = 1024, renderer = gifski_renderer())
  anim_save("animate_map_plot.gif")
  
}
