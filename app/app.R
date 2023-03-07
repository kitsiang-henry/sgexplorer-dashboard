#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages('shiny')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('vroom') #fast reading of csv files
install.packages('leaflet') #interactive maps
install.packages('htmlwidgets') #interactive map labels
install.packages('plotly')
install.packages('fontawesome')
install.packages('rsconnect')
install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('sentimentr')
install.packages('ggplot2')
install.packages('shinyBS')
install.packages('shinyhelper')
install.packages('rsconnect')
install.packages('terra')
install.packages('rgdal')
install.packages('Rcpp', dependencies = TRUE)
install.packages('sf')

library(shiny)
library(dplyr)
library(tidyverse)
library(vroom) #fast reading of csv files
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels
library(plotly)
library(fontawesome)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(ggplot2)
library(shinyBS)
library(shinyhelper)
library(terra)

#SMU EXCHANGE
#read in the SMU data
SMUrestaurants <- vroom("https://raw.githubusercontent.com/kitsiang-henry/sgexplorer-dashboard/main/data/SMURestaurants.csv")
SMUdf = data.frame(SMUrestaurants)

#Cuisines
by_price <- SMUdf%>% group_by(price)

#read in the Farrer data
FarrerRestaurants<-vroom("https://raw.githubusercontent.com/kitsiang-henry/sgexplorer-dashboard/main/data/FarrerRestaurants.csv")
Farrerdf= data.frame(FarrerRestaurants)

#read in the review data
SMU_reviews<-vroom("https://raw.githubusercontent.com/kitsiang-henry/sgexplorer-dashboard/main/data/SMU_reviews.csv")
SMU_reviews_df=data.frame(SMU_reviews)

Farrer_reviews<-vroom("https://raw.githubusercontent.com/kitsiang-henry/sgexplorer-dashboard/main/data/Farrer_reviews.csv")
Farrer_reviews_df=data.frame(Farrer_reviews)

#list of cuisines
cuisines=sort(unique(SMUdf$cuisine))

#BUSINESS
# Business Hotels
businessHotels <- "./www/shapefiles/business_hotels"
businessHotelsLayer <- vect(businessHotels, layer="business_hotels_shp")
businessHotelsCoords <- crds(businessHotelsLayer, df=TRUE)
businessHotelsData <- as.data.frame(businessHotelsLayer)
businessHotelsDF <- as.data.frame(businessHotelsLayer)

# Bars/Pubs
barsPubs <- "./www/shapefiles/bars_pubs"
barsPubsLayer <- vect(barsPubs, layer="bars_pubs_shp")
barsPubsCoords <- crds(barsPubsLayer, df=TRUE)
barsPubsData <- as.data.frame(barsPubsLayer)

# Restaurants
businessRestaurants <- "./www/shapefiles/restaurants"
businessRestaurantsLayer <- vect(businessRestaurants, layer="restaurants_shp")
businessRestaurantsCoords <- crds(businessRestaurantsLayer, df=TRUE)
businessRestaurantsData <- as.data.frame(businessRestaurantsLayer)
businessRestaurantsDF <- as.data.frame(businessRestaurantsLayer)
cuisinesRestaurants=sort(unique(businessRestaurantsDF$USER_Cuisi))

# Hawker Centres
hawkerCentres <- vroom("https://raw.githubusercontent.com/kitsiang-henry/sgexplorer-dashboard/main/data/hawker-centres.csv")
hawkerCentresDF <- data.frame(hawkerCentres)

#REQUIREMENTS
reqRestaurants<-vroom("https://raw.githubusercontent.com/kitsiang-henry/sgexplorer-dashboard/main/data/restaurant_requirements.csv")
reqRestaurants_df<-data.frame(reqRestaurants)
cafe_df<-reqRestaurants_df%>%dplyr::filter(cafe==TRUE)
fc_df<-reqRestaurants_df%>%dplyr::filter(food_court==TRUE)
halal_df<-reqRestaurants_df%>%dplyr::filter(halal==TRUE)
veg_df<-reqRestaurants_df%>%dplyr::filter(vegetarian==TRUE)
WC_df<-reqRestaurants_df%>%dplyr::filter(wheelchair_accessible==TRUE)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("SGExplorer"),
    
    tabsetPanel(
        
    tabPanel("SMU Exchange Students",
    # Sidebar
    sidebarLayout(
        
        sidebarPanel(
        # Checkboxes for Cuisine
            
        textInput("keyword", label=h5("Search for Restaurants")),
        
        
        checkboxGroupInput("checkCuisines", label = h3("Cuisine"), 
                           choices = cuisines,
                           selected = "Singaporean"),
        
        hr(),
        
        selectInput("Price",
                    "Select a price category ($, $$, $$$)",
                    choices=sort(unique(SMUrestaurants$price)),
                    selected="$"),
        
        hr(),
        #add how it is measured
        sliderInput("weightPopularity", "How much does a restaurant's popularity matter to you?", 1, 10, 5),
        
        h6("Note: Restaurant popularity is based on number of reviews")
        
        ),
        
        # Main Panel
        mainPanel(
                h1("Restaurant Finder"),
                bsCollapse(id="SMU Collapsibles", open="Restaurant Distribution",
                    bsCollapsePanel("Restaurant Distribution", div("This plot shows the number of restaurants across different cuisines for your selected price band. You may use it to see which cuisine you want to filter by."), HTML("<br>"),column(12,plotlyOutput("SMUbar"))),
                    bsCollapsePanel("Restaurant Around SMU in a Map", column(12,div("Use this interactive map to explore restaurants around SMU. A number in the bubble represents a cluster of restaurants. Feel free to click it to Zoom in to see each restaurant individually. You can also click on a bubble to find out more about the restaurant! Happy exploring :)"),HTML("<br>"), leafletOutput("SMUMap"))),
                    bsCollapsePanel("Restaurant Review Word Cloud", div(paste0("This plot shows the top words in reviews for the filtered restaurants. The bigger the word, the more frequently it appears in reviews. Examine the words to see what people are saying about certain restaurants serving certain cuisines and with a certain price band.")),column(12, plotOutput("SMUWordCloud"))),
                    bsCollapsePanel("Sentiment Distribution", div(actionButton("showBox1","How to Read Plot"), align="right"), HTML("</br>"),
                                    column(12, plotOutput("SMUSentiment")))
                ),
                hr(),
                h1("Explore Around Farrer Park"),
                bsCollapse(id="Farrer Park Collapsibles",
                    bsCollapsePanel("Restaurant Distribution", div("This plot shows the number of restaurants across different cuisines for your selected price band. You may use it to see which cuisine you want to filter by."), HTML("<br>"), column(12,plotlyOutput("Farrerbar"))),
                    bsCollapsePanel("Restaurant Around Farrer Park in a Map", div("Use this interactive map to explore restaurants around Farrer Park. A number in the bubble represents a cluster of restaurants. Feel free to click it to Zoom in to see each restaurant individually. You can also click on a bubble to find out more about the restaurant! Happy exploring :)"), HTML("<br>"),column(12,leafletOutput("FarrerMap"))),
                    bsCollapsePanel("Restaurant Review Word Cloud", column(12, div("This plot shows the top words in reviews for the filtered restaurants. The bigger the word, the more frequently it appears in reviews. Examine the words to see what people are saying about certain restaurants serving certain cuisines and with a certain price band."),plotOutput("FarrerWordCloud"))),
                    bsCollapsePanel("Sentiment Distribution", div(actionButton("showBox2", "How to Read Plot"), align="right"), HTML("</br>"),
                                    column(12, plotOutput("FarrerSentiment")))
                )
            )
    )
    ),
    
    tabPanel("Business Tourists",
             # Sidebar
             sidebarLayout(
                 
                 sidebarPanel(
                     # Checkboxes for Cuisine
                     
                     
                     textInput("keywordHotels", label=h5("Search for Hotels")),
                     
                     checkboxGroupInput("checkEateriesBusiness", label = h3("Eateries"),
                                        choices = c("Restaurants", "Bars/Pubs", "Hawker Centres"),
                                        selected = "Restaurants")

                 ),
                 
                 # Main Panel
                 mainPanel(
                     h1("Hotels and Eateries Finder"),
                     bsCollapse(id="Business Collapsibles", open="Hotels, Restaurants, Bars/Pubs and Hawker Centres",
                                bsCollapsePanel("Hotels, Restaurants, Bars/Pubs and Hawker Centres", 
                                                column(12,div("Use this interactive map to explore hotels, restaurants, bars/pubs or hawker centres that may interest you around Singapore. You can click on a bubble to find out more about the hotels, restaurants, bars/pubs or hawker centres! Happy exploring :)"), 
                                                HTML("<br>"), 
                                                div("LEGEND:"),
                                                div("Blue - Hotels, Red/Orange - Restaurants, Green - Bars/Pubs, Purple - Hawker Centres"),
                                                HTML("<br>"),
                                                leafletOutput("RestaurantsBarsPubsMap")))
                     )
                 )
                 
             )
    ),
    
    tabPanel("Halal",
             # Sidebar
             sidebarLayout(
                 
                 sidebarPanel(
                     # Checkboxes for Cuisine
                     
                     textInput("keywordHalal", label=h5("Search for Restaurants")),
                     
                     
                     checkboxGroupInput("checkCuisinesHalal", label = h3("Cuisine"), 
                                        choices = cuisines,
                                        selected = "Singaporean"),
                     
                     hr(),
                     
                     selectInput("PriceHalal",
                                 label="Price Category",
                                 choices=sort(unique(halal_df$price))),
                     
                     hr(),
                     
                     #add how it is measured
                     sliderInput("weightPopularityHalal", "How much does a restaurant's popularity matter to you?", 1, 10, 5),
                     
                     h6("Note: Restaurant popularity is based on number of reviews")
                     
                 ),
                 
                 # Main Panel
                 mainPanel(
                     h1("Halal Restaurant Finder"),
                     bsCollapse(id="Halal Collapsibles", open="Halal Restaurant",
                                bsCollapsePanel("Halal Restaurant", column(12,div("Use this interactive map to explore restaurants that may interest you around Singapore. A number in the bubble represents a cluster of restaurants. Feel free to click it to Zoom in to see each restaurant individually. You can also click on a bubble to find out more about the restaurant! Happy exploring :)"),HTML("<br>"), leafletOutput("HalalMap")))
                                #bsCollapsePanel("SMU Restaurant Distribution", div("This plot shows the number of restaurants across different cuisines for your selected price band. You may use it to see which cuisine you want to filter by."), HTML("<br>"),column(12,plotlyOutput("SMUbar"))),
                                #bsCollapsePanel("SMU Review Word Cloud", div(paste0("This plot shows the top words in reviews for the filtered restaurants. The bigger the word, the more frequently it appears in reviews. Examine the words to see what people are saying about certain restaurants serving certain cuisines and with a certain price band.")),column(12, plotOutput("SMUWordCloud"))),
                                #bsCollapsePanel("SMU Sentiment Distribution", div(actionButton("showBox1","How to Read Plot"), align="right"), HTML("</br>"), column(12, plotOutput("SMUSentiment")))
                     )
                 )
                 
             )
    ),
    
    tabPanel("Vegetarian",
             # Sidebar
             sidebarLayout(
                 
                 sidebarPanel(
                     # Checkboxes for Cuisine
                     
                     textInput("keywordVeg", label=h5("Search for Restaurants")),
                     
                     
                     checkboxGroupInput("checkCuisinesVeg", label = h3("Cuisine"), 
                                        choices = cuisines,
                                        selected = "Singaporean"),
                     
                     hr(),
                     
                     selectInput("PriceVeg",
                                 label="Price Category",
                                 choices=sort(unique(veg_df$price))),
                     
                     hr(),
                     
                     #add how it is measured
                     sliderInput("weightPopularityVeg", "How much does a restaurant's popularity matter to you?", 1, 10, 5),
                     
                     h6("Note: Restaurant popularity is based on number of reviews")
                     
                 ),
                 
                 # Main Panel
                 mainPanel(
                     h1("Vegetarian Restaurant Finder"),
                     bsCollapse(id="Veg Collapsibles", open="Vegetarian Restaurant",
                                bsCollapsePanel("Vegetarian Restaurant", column(12,div("Use this interactive map to explore restaurants that may interest you around Singapore. A number in the bubble represents a cluster of restaurants. Feel free to click it to Zoom in to see each restaurant individually. You can also click on a bubble to find out more about the restaurant! Happy exploring :)"),HTML("<br>"), leafletOutput("VegMap")))
                                #bsCollapsePanel("SMU Restaurant Distribution", div("This plot shows the number of restaurants across different cuisines for your selected price band. You may use it to see which cuisine you want to filter by."), HTML("<br>"),column(12,plotlyOutput("SMUbar"))),
                                #bsCollapsePanel("SMU Review Word Cloud", div(paste0("This plot shows the top words in reviews for the filtered restaurants. The bigger the word, the more frequently it appears in reviews. Examine the words to see what people are saying about certain restaurants serving certain cuisines and with a certain price band.")),column(12, plotOutput("SMUWordCloud"))),
                                #bsCollapsePanel("SMU Sentiment Distribution", div(actionButton("showBox1","How to Read Plot"), align="right"), HTML("</br>"), column(12, plotOutput("SMUSentiment")))
                     )
                 )
                 
             )
    ),
    tabPanel("Wheelchair Users",
             # Sidebar
             sidebarLayout(
                 
                 sidebarPanel(
                     # Checkboxes for Cuisine
                     
                     textInput("keywordWC", label=h5("Search for Restaurants")),
                     
                     
                     checkboxGroupInput("checkCuisinesWC", label = h3("Cuisine"), 
                                        choices = cuisines,
                                        selected = "Singaporean"),
                     
                     hr(),
                     
                     selectInput("PriceWC",
                                 label="Price Category",
                                 choices=sort(unique(WC_df$price))),
                     
                     hr(),
                     
                     #add how it is measured
                     sliderInput("weightPopularityWC", "How much does a restaurant's popularity matter to you?", 1, 10, 5),
                     
                     h6("Note: Restaurant popularity is based on number of reviews")
                     
                 ),
                 
                 # Main Panel
                 mainPanel(
                     h1("Wheelchair Accessible Restaurants Finder"),
                     bsCollapse(id="WC Collapsibles", open="Wheelchair Accessible Restaurant",
                                bsCollapsePanel("Wheelchair Accessible Restaurant", column(12,div("Use this interactive map to explore restaurants that may interest you around Singapore. A number in the bubble represents a cluster of restaurants. Feel free to click it to Zoom in to see each restaurant individually. You can also click on a bubble to find out more about the restaurant! Happy exploring :)"),HTML("<br>"), leafletOutput("WCMap")))
                                #bsCollapsePanel("SMU Restaurant Distribution", div("This plot shows the number of restaurants across different cuisines for your selected price band. You may use it to see which cuisine you want to filter by."), HTML("<br>"),column(12,plotlyOutput("SMUbar"))),
                                #bsCollapsePanel("SMU Review Word Cloud", div(paste0("This plot shows the top words in reviews for the filtered restaurants. The bigger the word, the more frequently it appears in reviews. Examine the words to see what people are saying about certain restaurants serving certain cuisines and with a certain price band.")),column(12, plotOutput("SMUWordCloud"))),
                                #bsCollapsePanel("SMU Sentiment Distribution", div(actionButton("showBox1","How to Read Plot"), align="right"), HTML("</br>"), column(12, plotOutput("SMUSentiment")))
                     )
                 )
                 
             )
    )
    )
    

)




# Define server logic
server <- function(input, output) {
    
    #SMU
    #possibly use an icon of a university instead
    SMUicon <- icons(
        iconUrl = "https://icons.iconarchive.com/icons/icons8/windows-8/512/Science-University-icon.png",
        iconWidth = 30, iconHeight = 30
    )
    
    uniIcon <- makeAwesomeIcon(
        icon = "building-columns",
        markerColor = "blue",
        library = "fa"
    )
    
    questionmarkIcon <- makeAwesomeIcon(
        icon= "question",
        markerColor="black",
        library="fa"
    )
    
    
    observeEvent(input$showBox1, {
        showModal(modalDialog(
            title = "How to read a Box-and-Whisker Plot",
            div(tags$img(src="https://aiaspirant.com/wp-content/uploads/2019/07/box-and-whisker-plot.jpg",height="400px", width="400px", deleteFile=TRUE)),
            easyClose = TRUE,
            footer = NULL
        )
        )
    })
    
    observeEvent(input$showBox2, {
        showModal(modalDialog(
            title = "How to read a Box-and-Whisker Plot",
            div(tags$img(src="https://aiaspirant.com/wp-content/uploads/2019/07/box-and-whisker-plot.jpg",height="400px", width="400px", deleteFile=TRUE)),
            easyClose = TRUE,
            footer = NULL
        )
        )
    })
    
    output$SMUbar<-renderPlotly({
        df<-SMUfiltered_by_price()
        groupbyCuisine<- df%>% count(cuisine, sort=TRUE)
        fig <- plot_ly(type = 'bar', width=450, color=I("darkred")) 
        fig <- fig %>%
            add_trace(
                x = groupbyCuisine$cuisine, 
                y = groupbyCuisine$n,
                #text = groupbyCuisine$cuisine,
                hoverinfo = groupbyCuisine$cuisine,
                #name= paste0("Restaurants By Cuisine (", input$Price, ")"),
                width=0.8,
                showlegend=FALSE
            )
        fig <- fig %>% layout(title= paste0("Number of Restaurants by Cuisine (",input$Price,")"), xaxis=list(title="Cuisine",tickangle=-35), yaxis=list(title="No. of Restaurants"))
    })
    
    output$Farrerbar<-renderPlotly({
        df<-Farrerfiltered_by_price()
        groupbyCuisine<- df%>% count(cuisine, sort=TRUE)
        fig <- plot_ly(type = 'bar', width=450, color=I("darkred")) 
        fig <- fig %>%
            add_trace(
                x = groupbyCuisine$cuisine, 
                y = groupbyCuisine$n,
                #text = groupbyCuisine$cuisine,
                hoverinfo = groupbyCuisine$cuisine,
                name= paste0("Restaurants By Cuisine (", input$Price, ")"),
                width=0.8,
                showlegend=FALSE
            )
        fig <- fig %>% layout(title=paste0("Number of Restaurants by Cuisine (", input$Price,")"), xaxis=list(title="Cuisine",tickangle=-35), yaxis=list(title="Number of Restaurants"))
    })
    
    output$SMUMap <- renderLeaflet({
        
        cuisine_df <- SMUfiltered_restaurants()
        pal <- colorNumeric(palette = c("Red", "Green"), domain = cuisine_df$rating)

        labels <- sprintf("<strong>%s</strong><br/> Restaurants around SMU",
                          input$checkCuisines)%>%
            lapply(htmltools::HTML)
        
        if(length(cuisine_df$name)==0) {
            map_interactive<- cuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8502, lat = 1.2963, zoom = 15.3) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addMarkers(lng= 103.8502, lat= 1.2963, popup= "SMU", label= "SMU", icon=SMUicon)
        }
        else {
            map_interactive<- cuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8502, lat = 1.2963, zoom = 15.3) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircles(lng= 103.8502, lat= 1.2963, radius=1000, color="White", opacity=1, label= "Within 1km of SMU", labelOptions=labelOptions(noHide=F, direction='top'))%>%
                addCircleMarkers(data= cuisine_df, lng = ~longitude, lat = ~latitude, popup = paste0("Restaurant name: ", as.character(cuisine_df$name),"<br>Price: ", cuisine_df$price, "<br>Cuisine: ", cuisine_df$cuisine, "<br>Rating: ", cuisine_df$rating, "<br>Address: ", cuisine_df$address), label = ~as.character(name), radius= ~(10*(review_count)^(input$weightPopularity/20)), color=~pal(rating), fillOpacity=0.95, clusterOptions=markerClusterOptions()) %>%
                addMarkers(lng= 103.8502, lat= 1.2963, popup= "SMU", label= "SMU", icon=SMUicon) %>%
                addLegend("bottomright", pal= pal, values = ~rating, title= "Rating")
            }

    })

    output$FarrerMap <- renderLeaflet({
        
        Farrercuisine_df <- Farrerfiltered_restaurants()
        
        pal <- colorNumeric(palette = c("Red", "Green"), domain = Farrercuisine_df$rating)
        
        labels <- sprintf("<strong>%s</strong><br/> Restaurants around Farrer Park",
                          input$checkCuisines)%>%
            lapply(htmltools::HTML)
        
        #use a conditional statement to print basemap or a display message to tell users to select an cuisine if no choices are selected
        #if there are no restaurants found, provide a message instead of the default error msg (e.g. for Indian)
        if(length(Farrercuisine_df$name)==0){
            map_interactive<- Farrercuisine_df %>%
                leaflet() %>%
                setView(lng=103.8542, lat = 1.3124, zoom = 16) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addMarkers(lng= 103.8542, lat= 1.3124, label= "Farrer Park MRT", labelOptions = labelOptions(noHide=T, direction='top'))
        }
        else{
            map_interactive<- Farrercuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8542, lat = 1.3124, zoom = 15) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircles(lng= 103.8542, lat= 1.3124, radius=1000, color="White", opacity=1, label= "Within 1km of Farrer park MRT", labelOptions=labelOptions(noHide=F, direction='top'))%>%
                addCircleMarkers(data= Farrercuisine_df, lng = ~longitude, lat = ~latitude, popup = paste0("Restaurant name: ", as.character(Farrercuisine_df$name),"<br>Price: ", Farrercuisine_df$price, "<br>Cuisine: ", Farrercuisine_df$cuisine, "<br>Rating: ", Farrercuisine_df$rating, "<br>Address: ", Farrercuisine_df$address), label = ~as.character(name), radius= ~(10*(review_count)^(input$weightPopularity/20)), color=~pal(rating), fillOpacity=0.9,clusterOptions=markerClusterOptions()) %>%
                addMarkers(lng= 103.8542, lat= 1.3124, label= "Farrer Park MRT", labelOptions = labelOptions(noHide=T, direction='top')) %>%
                addLegend("bottomright", pal= pal, values = ~rating, title= "Rating")
        }
    })
    
    output$SMUWordCloud <- renderPlot({
        SMU_reviews_df<- SMU_reviews_filtered()
        text_list<-list(SMU_reviews_df$review)
        text<-paste(unlist(text_list), collapse="")
        if(length(SMU_reviews_df$review)==0){
            print("Please select a cuisine. If you have already selected a cuisine, there may be no restaurants serving this cuisine around SMU. Please select another one (refer to bar chart above for available cuisines).")
        }
        else{
        #load the data as a corpus
        docs <- Corpus(VectorSource(text))
        docs <- tm_map(docs, content_transformer(tolower))
        # Remove numbers
        docs <- tm_map(docs, removeNumbers)
        # Remove english common stopwords
        docs <- tm_map(docs, removeWords, stopwords("english"))
        docs<-tm_map(docs, removeWords, c("food"))
        # Remove punctuations
        docs <- tm_map(docs, removePunctuation)
        # Eliminate extra white spaces
        docs <- tm_map(docs, stripWhitespace)
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        set.seed(1234)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=100, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
        }
    })
    
    output$FarrerWordCloud <- renderPlot({
        Farrer_reviews_df<- Farrer_reviews_filtered()
        text_list<-list(Farrer_reviews_df$review)
        text<-paste(unlist(text_list), collapse="")
        if(length(Farrer_reviews_df$review)==0){
            print("Please select a cuisine. If you have already selected a cuisine, there may be no restaurants serving this cuisine around Farrer Park MRT Station. Please select another one (refer to bar chart above for available cuisines).")
        }
        else{
        #load the data as a corpus
        docs <- Corpus(VectorSource(text))
        docs <- tm_map(docs, content_transformer(tolower))
        # Remove numbers
        docs <- tm_map(docs, removeNumbers)
        # Remove english common stopwords
        docs <- tm_map(docs, removeWords, stopwords("english"))
        docs<-tm_map(docs, removeWords, c("food"))
        # Remove punctuations
        docs <- tm_map(docs, removePunctuation)
        # Eliminate extra white spaces
        docs <- tm_map(docs, stripWhitespace)
        dtm <- TermDocumentMatrix(docs)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        set.seed(1234)
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=100, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
        }
    })
    
    output$SMUSentiment <- renderPlot({
        #insert image on how to read 
        SMU_reviews_df<- SMU_reviews_filtered()
        SMU_reviews_df<-SMU_reviews_df[c("review","cuisine")]
        SMU_reviews_df %>%
            get_sentences() %>%
            sentiment()-> SMU_sentiment
        SMU_sentiment%>%
            ggplot() + geom_boxplot(aes(x=cuisine, y=sentiment, color=sentiment)) + theme(panel.background = element_rect(fill='lightblue'), axis.text.x=element_text(size=15))
    })
    
    output$FarrerSentiment <-renderPlot({
        Farrer_reviews_df<-Farrer_reviews_filtered()
        Farrer_reviews_df<-Farrer_reviews_df[c("review", "cuisine")]
        Farrer_reviews_df %>%
            get_sentences()%>%
            sentiment()->Farrer_sentiment
        Farrer_sentiment%>%
            ggplot() + geom_boxplot(aes(x=cuisine, y=sentiment, color=sentiment)) + theme(panel.background = element_rect(fill='lightblue'), axis.text.x=element_text(size=15))
    })
    
    SMUfiltered_restaurants <- reactive({
        SMUdf %>% dplyr::filter(cuisine%in%input$checkCuisines) %>% filter(price==input$Price) %>% filter(grepl(tolower(input$keyword),tolower(name)))
    })
    
    SMUfiltered_by_price<- reactive({
        SMUdf%>%dplyr::filter(price==input$Price)
    })
    
    Farrerfiltered_restaurants <- reactive({
        Farrerdf %>% dplyr::filter(unlist(strsplit(cuisine, ","))%in%input$checkCuisines) %>% filter(price==input$Price) %>% filter(grepl(tolower(input$keyword),tolower(name)))
    })
    
    Farrerfiltered_by_price<- reactive({
        Farrerdf%>%dplyr::filter(price==input$Price)
    })
    
    SMU_reviews_filtered<- reactive({
        SMU_reviews_df%>%dplyr::filter(cuisine%in%tolower(input$checkCuisines)) %>% filter(price==input$Price) %>% filter(grepl(tolower(input$keyword),tolower(name)))
    })
    
    Farrer_reviews_filtered<- reactive({
        Farrer_reviews_df%>%dplyr::filter(cuisine%in%tolower(input$checkCuisines)) %>% filter(price==input$Price) %>% filter(grepl(tolower(input$keyword),tolower(name)))
    })

    
    #BUSINESS
    output$RestaurantsBarsPubsMap <- renderLeaflet({
        
        barsPubsPal <- colorNumeric(palette=c("Light Green", "Green"), domain=barsPubsData$USER_Ratin)
        businessHotelsPal <- colorNumeric(palette=c("Skyblue", "Blue"), domain=businessHotelsData$USER_Ratin)
        businessRestaurantsPal <- colorNumeric(palette=c("Red", "Orange"), domain=businessRestaurantsData$USER_Ratin)
        
        businessHotelsFiltered <- reactive({
            businessHotelsDF %>%
                dplyr::filter(grepl(tolower(input$keywordHotels),tolower(USER_Hotel)))
        })
        
        
        bizHotels <- businessHotelsFiltered()
        bizRes <- businessRestaurantsDF
        
        if (is.null(input$checkEateriesBusiness)) {
            map_interactive <- bizHotels %>%
                leaflet(businessHotelsCoords) %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
                addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35)
        }
        else if (length(input$checkEateriesBusiness) > 1) {
          if (length(input$checkEateriesBusiness) == 3) {
            map_interactive <- c(bizHotels, bizRes, hawkerCentresDF) %>%
              leaflet(businessHotelsCoords) %>%
              setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
              addProviderTiles(providers$OpenStreetMap) %>%
              addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
              addCircleMarkers(data=barsPubsCoords, lng=~x, lat=~y, popup=paste0("Bar/Pub name: ", as.character(barsPubsData$USER_Bars_), "<br>Price: ", barsPubsData$USER_Price, "<br>Ratings: ", barsPubsData$USER_Ratin, "<br>Address: ", barsPubsData$USER_Addre), color=~barsPubsPal(barsPubsData$USER_Ratin), fillOpacity=0.95) %>%
              addCircleMarkers(data=businessRestaurantsCoords, lng=~x, lat=~y, popup=paste0("Restaurant name: ", as.character(businessRestaurantsData$USER_Resta), "<br>Price: ", businessRestaurantsData$USER_Price, "<br>Cuisine: ", businessRestaurantsData$USER_Cuisi, "<br>Ratings: ", businessRestaurantsData$USER_Ratin, "<br>Address: ", businessRestaurantsData$USER_Addre), color=~businessRestaurantsPal(businessRestaurantsData$USER_Ratin), fillOpacity=0.95) %>%
              addCircleMarkers(data=hawkerCentresDF, lng=~X, lat=~Y, popup=paste0("Hawker Centre name: ", as.character(hawkerCentresDF$Name), "<br>Address: ", hawkerCentresDF$ADDRESS_MYENV), color="Purple", fillOpacity=0.95) %>%
              addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35) %>%
              addLegend("bottomright", pal=businessRestaurantsPal, values=businessRestaurantsData$USER_Ratin, title="Rating") %>%
              addLegend("bottomright", pal=barsPubsPal, values=barsPubsData$USER_Ratin, title="Rating")
          }
          else if (input$checkEateriesBusiness[1] == "Restaurants" & input$checkEateriesBusiness[2] == "Bars/Pubs") {
            map_interactive <- c(bizHotels, bizRes) %>%
              leaflet(businessHotelsCoords) %>%
              setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
              addProviderTiles(providers$OpenStreetMap) %>%
              addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
              addCircleMarkers(data=barsPubsCoords, lng=~x, lat=~y, popup=paste0("Bar/Pub name: ", as.character(barsPubsData$USER_Bars_), "<br>Price: ", barsPubsData$USER_Price, "<br>Ratings: ", barsPubsData$USER_Ratin, "<br>Address: ", barsPubsData$USER_Addre), color=~barsPubsPal(barsPubsData$USER_Ratin), fillOpacity=0.95) %>%
              addCircleMarkers(data=businessRestaurantsCoords, lng=~x, lat=~y, popup=paste0("Restaurant name: ", as.character(businessRestaurantsData$USER_Resta), "<br>Price: ", businessRestaurantsData$USER_Price, "<br>Cuisine: ", businessRestaurantsData$USER_Cuisi, "<br>Ratings: ", businessRestaurantsData$USER_Ratin, "<br>Address: ", businessRestaurantsData$USER_Addre), color=~businessRestaurantsPal(businessRestaurantsData$USER_Ratin), fillOpacity=0.95) %>%
              addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35) %>%
              addLegend("bottomright", pal=businessRestaurantsPal, values=businessRestaurantsData$USER_Ratin, title="Rating") %>%
              addLegend("bottomright", pal=barsPubsPal, values=barsPubsData$USER_Ratin, title="Rating")
          }
          else if (input$checkEateriesBusiness[1] == "Restaurants" & input$checkEateriesBusiness[2] == "Hawker Centres") {
            map_interactive <- c(bizHotels, bizRes, hawkerCentresDF) %>%
              leaflet(businessHotelsCoords) %>%
              setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
              addProviderTiles(providers$OpenStreetMap) %>%
              addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
              addCircleMarkers(data=hawkerCentresDF, lng=~X, lat=~Y, popup=paste0("Hawker Centre name: ", as.character(hawkerCentresDF$Name), "<br>Address: ", hawkerCentresDF$ADDRESS_MYENV), color="Purple", fillOpacity=0.95) %>%
              addCircleMarkers(data=businessRestaurantsCoords, lng=~x, lat=~y, popup=paste0("Restaurant name: ", as.character(businessRestaurantsData$USER_Resta), "<br>Price: ", businessRestaurantsData$USER_Price, "<br>Cuisine: ", businessRestaurantsData$USER_Cuisi, "<br>Ratings: ", businessRestaurantsData$USER_Ratin, "<br>Address: ", businessRestaurantsData$USER_Addre), color=~businessRestaurantsPal(businessRestaurantsData$USER_Ratin), fillOpacity=0.95) %>%
              addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35) %>%
              addLegend("bottomright", pal=businessRestaurantsPal, values=businessRestaurantsData$USER_Ratin, title="Rating")
          }
          else if (input$checkEateriesBusiness[1] == "Bars/Pubs" & input$checkEateriesBusiness[2] == "Hawker Centres") {
            map_interactive <- c(bizHotels, hawkerCentresDF) %>%
              leaflet(businessHotelsCoords) %>%
              setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
              addProviderTiles(providers$OpenStreetMap) %>%
              addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
              addCircleMarkers(data=hawkerCentresDF, lng=~X, lat=~Y, popup=paste0("Hawker Centre name: ", as.character(hawkerCentresDF$Name), "<br>Address: ", hawkerCentresDF$ADDRESS_MYENV), color="Purple", fillOpacity=0.95) %>%
              addCircleMarkers(data=barsPubsCoords, lng=~x, lat=~y, popup=paste0("Bar/Pub name: ", as.character(barsPubsData$USER_Bars_), "<br>Price: ", barsPubsData$USER_Price, "<br>Ratings: ", barsPubsData$USER_Ratin, "<br>Address: ", barsPubsData$USER_Addre), color=~barsPubsPal(barsPubsData$USER_Ratin), fillOpacity=0.95) %>%
              addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35) %>%
              addLegend("bottomright", pal=barsPubsPal, values=barsPubsData$USER_Ratin, title="Rating")
          }
        }
        else {
          if (input$checkEateriesBusiness == "Bars/Pubs") {
            map_interactive <- bizHotels %>%
                leaflet(businessHotelsCoords) %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
                addCircleMarkers(data=barsPubsCoords, lng=~x, lat=~y, popup=paste0("Bar/Pub name: ", as.character(barsPubsData$USER_Bars_), "<br>Price: ", barsPubsData$USER_Price, "<br>Ratings: ", barsPubsData$USER_Ratin, "<br>Address: ", barsPubsData$USER_Addre), color=~barsPubsPal(barsPubsData$USER_Ratin), fillOpacity=0.95) %>%
                addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35) %>%
                addLegend("bottomright", pal=barsPubsPal, values=barsPubsData$USER_Ratin, title="Rating")
          }
          else if (input$checkEateriesBusiness == "Restaurants") {
              map_interactive <- c(bizHotels, bizRes) %>%
                  leaflet(businessHotelsCoords) %>%
                  setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                  addProviderTiles(providers$OpenStreetMap) %>%
                  addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
                  addCircleMarkers(data=businessRestaurantsCoords, lng=~x, lat=~y, popup=paste0("Restaurant name: ", as.character(businessRestaurantsData$USER_Resta), "<br>Price: ", businessRestaurantsData$USER_Price, "<br>Cuisine: ", businessRestaurantsData$USER_Cuisi, "<br>Ratings: ", businessRestaurantsData$USER_Ratin, "<br>Address: ", businessRestaurantsData$USER_Addre), color=~businessRestaurantsPal(businessRestaurantsData$USER_Ratin), fillOpacity=0.95) %>%
                  addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35) %>%
                  addLegend("bottomright", pal=businessRestaurantsPal, values=businessRestaurantsData$USER_Ratin, title="Rating")
          }
          else if (input$checkEateriesBusiness == "Hawker Centres") {
              map_interactive <- c(bizHotels, hawkerCentresDF) %>%
                  leaflet(businessHotelsCoords) %>%
                  setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                  addProviderTiles(providers$OpenStreetMap) %>%
                  addCircles(lng=businessHotelsCoords$x, lat=businessHotelsCoords$y, radius=1000, color="Yellow", opacity=1, label= "Within 1km of hotel", labelOptions=labelOptions(noHide=F, direction='top')) %>%
                  addCircleMarkers(data=hawkerCentresDF, lng=~X, lat=~Y, popup=paste0("Hawker Centre name: ", as.character(hawkerCentresDF$Name), "<br>Address: ", hawkerCentresDF$ADDRESS_MYENV), color="Purple", fillOpacity=0.95) %>%
                  addCircleMarkers(data=businessHotelsCoords, lng=~x, lat=~y, popup=paste0("Hotel name: ", as.character(businessHotelsData$USER_Hotel), "<br>Address: ", businessHotelsData$USER_Addre, "<br>Ratings: ", businessHotelsData$USER_Ratin), color=~businessHotelsPal(businessHotelsData$USER_Ratin), fillOpacity=0.35)
          }
        }
        
    })
    
    #HALAL
    output$HalalMap <- renderLeaflet({
        
        Halalcuisine_df <- Halalfiltered_restaurants()
        pal <- colorNumeric(palette = c("Red", "Green"), domain = Halalcuisine_df$rating)
        
        labels <- sprintf("<strong>%s</strong><br/> Fine Dining in Singapore",
                          input$checkCuisinesBiz)%>%
            lapply(htmltools::HTML)
        
        if(length(Halalcuisine_df$name)==0) {
            map_interactive<- Halalcuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap)
        }
        else {
            map_interactive<- Halalcuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircleMarkers(data= Halalcuisine_df, lng = ~longitude, lat = ~latitude, popup = paste0("Restaurant name: ", as.character(Halalcuisine_df$name),"<br>Price: ", Halalcuisine_df$price, "<br>Cuisine: ", Halalcuisine_df$cuisine, "<br>Rating: ", Halalcuisine_df$rating, "<br>Address: ", Halalcuisine_df$address), label = ~as.character(name), radius= ~(10*(review_count)^(input$weightPopularityHalal/20)), color=~pal(rating), fillOpacity=0.95, clusterOptions=markerClusterOptions()) %>%
                addLegend("bottomright", pal= pal, values = ~rating, title= "Rating")
        }
        
    })
    
    Halalfiltered_restaurants <- reactive({
        halal_df %>% dplyr::filter(cuisine%in%input$checkCuisinesHalal) %>% filter(price==input$PriceHalal)%>% filter(grepl(tolower(input$keywordHalal),tolower(name)))
    })
    
    #VEGETARIAN
    output$VegMap <- renderLeaflet({
        
        Vegcuisine_df <- Vegfiltered_restaurants()
        pal <- colorNumeric(palette = c("Red", "Green"), domain = Vegcuisine_df$rating)
        
        labels <- sprintf("<strong>%s</strong><br/> Fine Dining in Singapore",
                          input$checkCuisinesVeg)%>%
            lapply(htmltools::HTML)
        
        if(length(Vegcuisine_df$name)==0) {
            map_interactive<- Vegcuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap)
        }
        else {
            map_interactive<- Vegcuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircleMarkers(data= Vegcuisine_df, lng = ~longitude, lat = ~latitude, popup = paste0("Restaurant name: ", as.character(Vegcuisine_df$name),"<br>Price: ", Vegcuisine_df$price, "<br>Cuisine: ", Vegcuisine_df$cuisine, "<br>Rating: ", Vegcuisine_df$rating, "<br>Address: ", Vegcuisine_df$address), label = ~as.character(name), radius= ~(10*(review_count)^(input$weightPopularityVeg/20)), color=~pal(rating), fillOpacity=0.95, clusterOptions=markerClusterOptions()) %>%
                addLegend("bottomright", pal= pal, values = ~rating, title= "Rating")
        }
        
    })
    
    
    Vegfiltered_restaurants <- reactive({
        veg_df %>% dplyr::filter(cuisine%in%input$checkCuisinesVeg) %>% filter(price==input$PriceVeg)%>%filter(grepl(tolower(input$keywordVeg),tolower(name)))
    })
    
    #WHEELCHAIR
    output$WCMap <- renderLeaflet({
        
        WCcuisine_df <- WCfiltered_restaurants()
        pal <- colorNumeric(palette = c("Red", "Green"), domain = WCcuisine_df$rating)
        
        labels <- sprintf("<strong>%s</strong><br/> Fine Dining in Singapore",
                          input$checkCuisinesWC)%>%
            lapply(htmltools::HTML)
        
        if(length(WCcuisine_df$name)==0) {
            map_interactive<- WCcuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap)
        }
        else {
            map_interactive<- WCcuisine_df %>%
                leaflet() %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                addProviderTiles(providers$OpenStreetMap) %>%
                addCircleMarkers(data= WCcuisine_df, lng = ~longitude, lat = ~latitude, popup = paste0("Restaurant name: ", as.character(WCcuisine_df$name),"<br>Price: ", WCcuisine_df$price, "<br>Cuisine: ", WCcuisine_df$cuisine, "<br>Rating: ", WCcuisine_df$rating, "<br>Address: ", WCcuisine_df$address), label = ~as.character(name), radius= ~(10*(review_count)^(input$weightPopularityWC/20)), color=~pal(rating), fillOpacity=0.95, clusterOptions=markerClusterOptions()) %>%
                addLegend("bottomright", pal= pal, values = ~rating, title= "Rating")
        }
        
    })
    
    WCfiltered_restaurants <- reactive({
        WC_df %>% dplyr::filter(cuisine%in%input$checkCuisinesWC) %>% filter(price==input$PriceWC)%>%filter(grepl(tolower(input$keywordWC),tolower(name)))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
