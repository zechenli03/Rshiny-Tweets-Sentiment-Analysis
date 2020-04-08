# title: "IE6600 - Group 8 Project"
# author: "Zechen Li"



ui <- fluidPage(
  theme = shinythemes::shinytheme("cyborg"),

  
  titlePanel(fluidRow(
    column(width = 3, tags$img(src = "logo.svg",height='120')),
    column(width = 9, h1("Visulization whth Twitter Hashtags")))
  ),

  
  br(),
  
  sidebarPanel(
    width = 4,
    textInput(inputId = "hashtag",
              label = "Enter Hashtag to be searched with '#':",
              value='coronavirus',
              width = '90%'),
    br(),
    sliderInput(inputId='number',
                label="Number of recent tweets to use for analysis:",
                value=2000,
                min=100,
                max=3000,
                width = '90%',
                step = 100), 
    br(),
    submitButton(text="Visualize")
    
  ),
  
  mainPanel(
    tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #b6b6b6;
                    } 
                    .dataTables_length select {
                           color: #b6b6b6;
                           background-color: #313131
                    }
                    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
                    background: black;
                    color: white!important;
                    border-radius: 4px;
                    border: 1px solid #828282;
                    }
                    .dataTables_wrapper .dataTables_paginate .paginate_button:active {
                    background: black;
                    color: black!important;
                    }
                    
                    .dataTables_wrapper .dataTables_paginate .paginate_button {
                    background: black;
                    color: white!important;
                    }
                    ")),
    
    tabsetPanel(
      tabPanel("Trending Topics Today in U.S",
               br(),
               fluidRow(
                 column(width = 7),
                 column(width = 4, selectInput(inputId="location",
                                               label="Choose location to extract hashtag:",
                                               c("Seattle" = "2490383","New York" = "2459115", "Chicago" = "2379574","Las Vegas" = "2436704",
                                                 "Houston" = "2424766", "Boston" = "2367105","Detroit" = "2391585",
                                                 "Charlotte" = "2378426","Los Angeles" = "2442047","San Diego" = "2487889"), 
                                               selected = "Worldwide", selectize = FALSE,width = '80%')),
                 column(width = 1, br(), submitButton(text="Search"))),br(),
               fluidRow(
                 column(4),
                 column(8,"The top 10 Trending Topics: ",
                        br(),br(),tableOutput("trendtable"))
               )),
      
      tabPanel('Wordclouds for Hashtags', br(),
               fluidRow(
                 column(width = 8),
                 column(width = 3, selectInput(inputId="language",
                                               label="Choose tweets' language:",
                                               c("All" = "Null",
                                                 "English" = "en",
                                                 "Simplified Chinese" = 'zh-cn',
                                                 "Traditional Chinese" = 'zh-tw',
                                                 "Indonesian" = "id",
                                                 "French" = "fr",
                                                 "Japanese" = "ja",
                                                 "Korean" = "ko"), selected = "All", selectize = FALSE)),
                 column(width = 1, br(), submitButton(text="Show"))),
               br(),
               "Most frequent hashtags fromt the tweet hashtag list based on a given hashtag:",
               br(), br(),
               wordcloud2Output(outputId = "wordcloud1", height = "600px")
               ),
      
      tabPanel('Wordclouds for Text',br(),
               "Most used words from the tweets based on a given hashtag (English):",
               br(),br(),
               wordcloud2Output(outputId = "wordcloud2", height = "600px")
      ),
      
      tabPanel("Analyzed Tweets",br(),
               "The table describes the sentiment (positive, negative, or neutral) of the tweets associated with the searched Hashtag 
               by showing a score for each type of sentiment.",br(),br(),
               DT::dataTableOutput("tweetstable"),br(),br()),
      
      tabPanel("Sentiment Plots",br(),br(),
               plotOutput("histPos"), br(),br(),
               plotOutput("histNeg"), br(),br(),
               plotOutput("histScore"),br(),br(),
               fluidRow(
                 column(12,align = "center",
                        plotOutput(outputId='piechart', width = "410px")
                 )
               )
      ),
      
      tabPanel('World Map', br(), textOutput("title"), br(), 
               leafletOutput(outputId = "map", height = "600px")),
      
      tabPanel('Animated Map', br(), 
               "(Generation often takes 1~3 minutes)",
               br(), textOutput("title2"), br(), 
               fluidRow(
                 column(12, align="center",
                        imageOutput(outputId='animate_map')
                 )
               ))
    )
  )
  
  
)