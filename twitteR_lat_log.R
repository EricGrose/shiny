library(leaflet)
library(twitteR)
library(shiny)
library(sentimentr)

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(4, textInput("searchkw", label = "search:", value = "#movie")),
      column(4, textInput("lat", label = "latitude:", value = 40.75)),
      column(4, textInput("long", label = "longitude:", value = -74)),
      column(8, leafletOutput("myMap")),
      column(12, tableOutput('table'))
    )
  ),
  server = function(input, output) {
    
    # OAuth authentication
    consumer_key <- "consumer_key"
    consumer_secret <- "consumer_secret"
    access_token <- "access_token"
    access_secret <- "access_secret"
    options(httr_oauth_cache = TRUE) # enable using a local file to cache OAuth access credentials between R sessions
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
    # Issue search query to Twitter
    dataInput <- reactive({  
      tweets <- twListToDF(searchTwitter(input$searchkw, n = 100, 
                                         geocode = paste0(input$lat, ",", input$long, ",10km")))
      tweets$text <- gsub('[[:punct:] ]+',' ',tweets$text)
      tweets$text <- gsub("http.*"," ",  tweets$text)
      tweets$text <- gsub("https.*"," ", tweets$text)
      #tweets$sentiment <- sentiment(tweets$text)
      tweets$created <- as.character(tweets$created)
      tweets <- tweets[!is.na(tweets[, "longitude"]), ]
    })
    
    # Create a reactive leaflet map
    mapTweets <- reactive({
      map = leaflet() %>% addTiles() %>%
        addMarkers(as.numeric(dataInput()$longitude), as.numeric(dataInput()$latitude), popup = dataInput()$screenName) %>%
        setView(input$long, input$lat, zoom = 11)
    })
    output$myMap = renderLeaflet(mapTweets())
    
    # Create a reactive table 
    output$table <- renderTable(
      dataInput()[, c("text", "screenName", "longitude", "latitude", "created")]
    )
  }
)
