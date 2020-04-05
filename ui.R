# title: "IE6600 - Group 8 Project"
# author: "Zechen Li"



ui <- fluidPage(
  theme = shinythemes::shinytheme("cyborg"),
  
  tags$head(tags$style("div.dataTables_scrollHead span {color: black;}")),
  
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
                value=500,
                min=100,
                max=5000,
                width = '90%'), 
    br(),
    selectInput(inputId="trendingTable",
                label="Choose location to extract hashtag:",
                c("Worldwide","Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Anguilla","Antarctica",
                  "Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain",
                  "Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bermuda","Bhutan","Bolivia","Brazil",
                  "British Indian Ocean Territory","Bulgaria","Burundi","Cambodia","Cameroon","Canada","Cayman Islands",
                  "Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo","Cook Islands","Costa Rica",
                  "Croatia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","Ecuador",
                  "Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Falkland Islands","Faroe Islands",
                  "Fiji","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Gibraltar","Greece","Greenland",
                  "Grenada","Guadeloupe","Guam","Guatemala","Guinea","Guinea-bissau","Guyana","Vatican City","Honduras",
                  "Hong Kong","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica",
                  "Japan","Jordan","Kazakhstan","Kenya","Kiribati","Democratic People's Republic of Korea","Republic of Korea",
                  "Kuwait","Kyrgyzstan","Lao People's Democratic Republic","Latvia","Lebanon","Lesotho","Liberia",
                  "Libyan Arab Jamahiriya","Liechtenstein","Lithuania","Luxembourg","Macao","Macedonia","Madagascar","Malawi","Malaysia",
                  "Maldives","Mali","Malta","Marshall Islands","Martinique","Mauritania","Mauritius","Mayotte","Mexico",
                  "Micronesia","Republic of Moldova","Monaco","Mongolia","Montserrat","Morocco","Mozambique","Myanmar",
                  "Namibia","Nauru","Nepal","Netherlands","Netherlands Antilles","New Caledonia","New Zealand","Nicaragua",
                  "Niger","Nigeria, Niue","Norfolk Island","Northern Mariana Islands","Norway","Oman","Pakistan","Palau",
                  "Palestinian Territory Occupied","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Pitcairn",
                  "Poland","Portugal","Puerto Rico","Qatar","Reunion","Romania","Russian Federation","Rwanda","Saint Helena",
                  "Saint Kitts and Nevis","Saint Lucia","Saint Pierre and Miquelon","Saint Vincent and The Grenadines","Samoa",
                  "San Marino","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia and Montenegro","Seychelles","Sierra Leone",
                  "Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Georgia and The South Sandwich Islands",
                  "Spain","Sri Lanka","Sudan","Suriname","Svalbard and Jan Mayen","Swaziland","Sweden","Switzerland",
                  "Syrian Arab Republic","Taiwan","Province of China","Tajikistan","United Republic of Tanzania","Thailand",
                  "Timor-leste","Togo","Tokelau","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Turks and Caicos Islands",
                  "Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","United States Minor Outlying Islands",
                  "Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Virgin Islands","British","Virgin Islands","U.S.",
                  "Wallis and Futuna","Western Sahara","Yemen","Zambia", "Zimbabwe"), selected = "Worldwide", selectize = FALSE,width = '65%'),
    br(),
    submitButton(text="Visualize")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Top Trending Topics Today",
               br()),
      
      tabPanel('Wordclouds for hashtags', br(),
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
                 column(width = 1, br(), submitButton(text="Visualize"))),
               br(),
               "Most frequent hashtags fromt the tweet hashtag list based on a given hashtag:",
               br(), br(),
               wordcloud2Output(outputId = "wordcloud1", height = "600px")
               ),
      
      tabPanel('Wordclouds for text',br(),
               "Most used words from the tweets based on a given hashtag (English):",
               br(),br(),
               wordcloud2Output(outputId = "wordcloud2", height = "600px")
      ),
      
      tabPanel('Frequency Table',DT::dataTableOutput(outputId = "table")),
      
      tabPanel('Tweets',DT::dataTableOutput(outputId='txt'))
    )
  )
  
  
)