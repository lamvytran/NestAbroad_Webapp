#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# new r script

library(shiny)
library(bslib)
library(readxl)
library(leaflet)
library(leaflet.extras) 
library(shinycssloaders)
library(ggmap)
library(wordcloud2)
library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(cld3)
library(sf)
library(fmsb)

#load in dataset:
load('Cleaned Dataset/listings.Rda') 

ubc <- read_excel("Cleaned Dataset/UBC Campus.xlsx", 
                  sheet = "Dataset")

#reviews dataset
reviews <- read.csv("Cleaned Dataset/reviews_filtered.csv")

#community centre dataset
community.centres.data <- read.csv("Cleaned Dataset/community_centres.csv")


#hospital
hospitals.data <- read_excel("Raw Dataset/Vancouver Hospital Facilities.xlsx")

#supermarket 
supermarkets.data <- read_excel("Raw Dataset/Vancouver Supermarket Data.xlsx")

#prices
prices <- read.csv("Cleaned Dataset/UBC_Prices_Complied.csv")

# Vancouver Map 
ggmap::register_google(key = 'AIzaSyC4aB4IEU9F0bwoQaXHDf2ZYdi-uUa8CJM')
map<-get_map("vancouver", zoom =12, source="google")
ggmap(map) + geom_point(data=listings,aes(x=longitude,y=latitude))

#UI
ui <- fluidPage(
  
  navbarPage(
    "NestAbroad",
    theme = bs_theme(version = 5, bootswatch = "lumen"),
    tags$head(tags$style(HTML(".navbar-default { background-color: lightblue; }"))),
    tabPanel("About Us",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 img(src = "logo.png", height=400, width=380), 
                 br(),
                 br(),
                 br(),
                 strong("Welcome to NestAbroad!"), 
                 p("Your one stop solution for all your housing needs during your Student Exchange Program to Vancouver, Canada! We understand the struggles of finding an accommodation, especially when there are a myriad of options available out there.", style = "font-family: times; font-si18pt"),
                 br(),
                 p("In this application, you, an NUS student embarking on a Student Exchange Program to Vancouver, Canada, will be able to source for suitable accomodations according to what matters most to you.", style = "font-family: times; font-si18pt"),
                 br(),
                 p("You will be able to narrow down your options based on location, number of bedrooms, bathrooms, and others.", style = "font-family: times; font-si18pt")
               )
             )
    ),
    
    navbarMenu("Accommodation Overview",
               tabPanel("External",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Instructions: Vary the Distance from School, Minimum Number of Nights and Price Range to check out which Airbnb Listings are available and where are they located."),
                            br(),
                            sliderInput(inputId = "distance_from_school", h3("Distance from School (m)"),
                                        min = 100, max = 20000, value = 100), #distance is in metres
                            sliderInput(inputId = "slider1", h3("Minimum Number of Nights"),
                                        min = 50, max = 100, value = 50),
                            sliderInput(inputId = "priceRange", h3("Price Range (SGD$ / night)"),
                                        min = 0, max = 200, value = c(0, 200)),
                            br(),
                            helpText("Below are the list of neighbourhoods in Vancouver. You are welcome to select multiple neighbourhoods which interest you to find the perfect nest that will make your heart sing."),
                            checkboxGroupInput(inputId = "neighbourhoods", h3("Neighbourhoods"),
                                               choices = unique(listings$neighbourhood_cleansed),
                                               selected = unique(listings$neighbourhood_cleansed))
                          ),
                          
                          mainPanel(
                            tags$h3(style="font-weight:bold", "Find your perfect nest here"),
                            h5("Ruffle through our filters using the sidebar."),
                            h6("Soar over the Airbnb listings with a click of your mouse to see the listing names, ID and price!"),
                            fluidRow(
                              withSpinner(leafletOutput("ScatterListings")),
                              withSpinner(plotOutput("radar")),
                              column(width = 12, tags$h3(style="font-weight:bold", "Nest Count"),
                                     h5("Flap over here to see the extraordinary number of nests across Vancouver based on room type."),
                                     helpText("Scroll your mouse over the Nest Count geo-map to zoom in on the room type breakdowns in the neighbourhoods."),
                                     br(),
                                     h6("Red markers indicates listings with the room type of 'Entire home/ apt'."),
                                     h6("Green markers indicates listings with the room type of 'Private room'."),
                                     h6("Blue markers indicates listings with the room type of 'Hotel room'."),
                                     h6("Yellow markers indicates listings with the room type of 'Shared room'."),
                                     withSpinner(leafletOutput("m1"))),
                              
                            )
                          )
                        )),
               
               tabPanel("Campus",
                        
                        sidebarLayout(
                          sidebarPanel (
                            helpText("Instructions: Select the relevant Category and Accomodation Period for your Exchange!"),
                            br(),
                            br(),
                            selectInput(inputId = "category", label = "Category of Accommodation", choices = c("-", "First Year", "Upper Year")), #input needs to be unique
                            selectInput(inputId = "session", label = "Accomodation Period", choices = c("-", "Winter", "Summer", "Year Round"))
                          ), 
                          
                          mainPanel(withSpinner(leafletOutput("MapAccom"))) 
                        ))),
    
    tabPanel("External Accommodation",
             
             sidebarLayout(
               sidebarPanel(
                 helpText("Instructions: Search the Listing ID to get a general idea of the Accomodation as well as the nearest Facilities."),
                 br(),
                 helpText("Notes: Under the Explore Neighborhood tab, the Blue Dot is  Nearest Supermarket, The Red Dot is Nearest Hospital, The Green Dot is Nearest Community Centres."),
                 br(),
                 br(),
                 selectInput(inputId = "listing_name", label = "Select a Listing ID", choices = unique(reviews$listing_id)) 
                 
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Word Cloud", withSpinner(wordcloud2Output("WordCloud"))),
                   tabPanel("Explore Neighborhood",withSpinner(leafletOutput("explore",width = "100%", height = "500px"))))) 
             )),
               

    tabPanel("Campus Accommodation",
             tags$style(".row{height: 700px;} .row div:nth-child(1){height: 100%;}"),
             navlistPanel(
               "Campus Accommodation",
               tabPanel("Orchard Commons",
                        h3("Description"),
                        p("Orchard Commons is one of our newest residences and home to Vantage College. It’s ideal for students who are new to UBC and want space and time to study, as well as a relaxed supportive atmosphere perfect for making new friends. Everyone at “Orchard” purchases a convenient Residence Meal Plan."),
                        h3("Facilities"),
                        p("Dining Room, Fitness Room, Music Room, Study Spaces, Games Lounge, Waste Sorting, Stations, Laundry, Courtyard, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/orchard-commons/", 
                               "Click here for more information"),
                        img(src='orchardcommon.jpeg', align = "center", height=600, width=800)),
               tabPanel("Place Vanier",
                        h3("Description"),
                        p("Place Vanier is where the university experience begins for new undergraduate students—domestic and international. Everyone at “Vanier” buys a Residence Meal Plan so there’s no need to cook or clean dirty dishes. Enjoy lots of study spaces and nearby public beaches."),
                        h3("Facilities"),
                        p("Dining Room, Fitness Room, Music Room, Study Spaces, Games Lounge, Waste Sorting, Stations, Laundry, Courtyard, Medical Clinic, Convenience Store, Tennis Court, Basketball Court, Park"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/place-vanier/", 
                               "Click here for more information"),
                        img(src='placevanier.jpeg', align = "center", height=600, width=800)),
               tabPanel("Totem Park",
                        h3("Description"),
                        p("Totem Park is a residence primarily for new undergraduate and international students with minimal university experience. It’s the the perfect place for first year students to make connections and find support services. Everyone at “Totem” purchases a convenient Residence Meal Plan."),
                        h3("Facilities"),
                        p("Dining Room, Fitness Room, Music Room, Study Spaces, Games Lounge, Waste Sorting Stations, Laundry, Courtyard, Medical Clinic, Convenience Store, Tennis Court, Basketball Court, Park"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/totem-park/", 
                               "Click here for more information"),
                        img(src='totempark.jpeg', align = "center", height=600, width=800)),
               tabPanel("Brock Commons- Tallwood House",
                        h3("Description"),
                        p("Brock Commons – Tallwood House features a central location, spectacular views and unique building features. The AMS Nest, bus exchange, and many campus classrooms and recreational facilities are nearby."),
                        h3("Facilities"),
                        p("Meeting Room, Study Space, Lounge, Convenience Store, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/brock-commons/", 
                               "Click here for more information"),
                        img(src='brock.jpeg', align = "center", height=600, width=800)),
               tabPanel("Exchange",
                        h3("Description"),
                        p("Home to over 650 students, Exchange offers an active, urban energy and a uniquely connected location. It is located alongside the UBC bus exchange and just steps from the UBC Aquatic Centre and Student Recreation Centre."),
                        h3("Facilities"),
                        p("Meeting Room, Study Space, Lounge, Exercise Room, Music Practice Room, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Medical Clinic, Park"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/exchange/", 
                               "Click here for more information"),
                        img(src='exchange.jpeg', align = "center", height=600, width=800)),
               tabPanel("Fairview Crescent",
                        h3("Description"),
                        p("Live with other upper year undergraduate students in a vibrant, friendly neighbourhood. Fairview Crescent’s fully furnished, townhouse-style suites are conveniently located on the east side of campus near UBC’s two main shopping areas."),
                        h3("Facilities"),
                        p("Fitness Room, Study Space, Multipurpose Room, Lounge, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Park, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/fairview-crescent/", 
                               "Click here for more information"),
                        img(src='fairview.jpeg', align = "center", height=600, width=800)),
               tabPanel("Fraser Hall",
                        h3("Description"),
                        p("Mainly upper year and graduate students live here in fully furnished six bedroom suites. Conveniently located near University Village, Fraser Hall residents get bi-weekly housekeeping service."),
                        h3("Facilities"),
                        p("Lounge, Park, Waste Sorting Stations, Laundry, Housekeeping, Basketball Court, Tennis Court, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/fraser-hall/", 
                               "Click here for more information"),
                        img(src='fraser.jpeg', align = "center", height=600, width=800)),
               tabPanel("Iona House",
                        h3("Description"),
                        p("Upper year and graduate students at Iona House live near UBC University Village in a small residence community of only 20 units. This beautiful old heritage building is also home to the UBC School of Economics."),
                        h3("Facilities"),
                        p("Activity Room, Meeting Room, Study Space, Lounge, Convenience Store, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Park, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/iona-house/", 
                               "Click here for more information"),
                        img(src='iona.jpeg', align = "center", height=600, width=800)),
               tabPanel("Marine Drive",
                        h3("Description"),
                        p("Located on the quiet west side of campus, Marine Drive offers a relaxed, communal atmosphere mainly for upper year and graduate students. Fully furnished suites with floor-to-ceiling windows can offer spectacular views of the mountains and ocean."),
                        h3("Facilities"),
                        p("Fitness Room, Study Space, Meeting Room, Ballroom, Fireside Lounge, Restaurant, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Park, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/marine-drive-residence/", 
                               "Click here for more information"),
                        img(src='marine.jpeg', align = "center", height=600, width=800)),
               tabPanel("Ponderosa Commons",
                        h3("Description"),
                        p("Located in the heart of campus, it’s a popular place for students to socialize, study, dine, and live. Ponderosa Commons was built sustainably to reduce waste and energy use. All suites are fully furnished and come with Wi-Fi and a dishwasher."),
                        h3("Facilities"),
                        p("Fitness Room, Study Space, Meeting Room, Ballroom, Fireside Lounge, Restaurant, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Park, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/ponderosa-commons/", 
                               "Click here for more information"),
                        img(src='ponderosa.jpeg', align = "center", height=600, width=800)),
               tabPanel("The Houses of the Ones Belonging to the Saltwater",
                        h3("Description"),
                        p("Is our newest residence community at UBC. In fall 2021, the first two houses opened and became home to nearly 400 students. Once the remaining three houses are complete, this community will provide housing for 884 students in a vibrant and dynamic section of campus, alongside Walter Gage, Tallwood House and Exchange."),
                        h3("Facilities"),
                        p("Fitness Room, Music Practice Room, Study Spaces, Games Room, Lounge, Waste Sorting Stations, Laundry, Courtyard, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/houses-of-ones-belonging-to-saltwater/", 
                               "Click here for more information"),
                        img(src='salt.jpeg', align = "center", height=600, width=800)),
               tabPanel("Ritsumeikan-UBC House",
                        h3("Description"),
                        p("A mix of new and returning undergraduate students live here in fully furnished suites on the west side of campus. “Rits” residents share a Commonsblock with Orchard Commons and have easy access to green space, nearby trails, and public beaches."),
                        h3("Facilities"),
                        p("Dining Room, Fitness Room, Music Room, Study Spaces, Ballroom, Games Lounge, Convenience Store, Waste Sorting Stations, Laundry, Courtyard, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/ritsumeikan-ubc-house/", 
                               "Click here for more information"),
                        img(src='ritsumeikan.jpg', align = "center", height=600, width=800)),
               tabPanel("Thunderbird",
                        h3("Description"),
                        p("Upper year and graduate students at Thunderbird enjoy private-entrance condo living in a quiet, modern neighbourhood. Residents have easy access to green space, nearby trails and public beaches."),
                        h3("Facilities"),
                        p("Fitness Room, Music Practice Room, Lounge, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Park, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/thunderbird/", 
                               "Click here for more information"),
                        img(src='thunderbird.jpeg', align = "center", height=600, width=800)),
               tabPanel("Walter Gage",
                        h3("Description"),
                        p("“Gage” is well known for its positive energy and superb location near the bus exchange and recreation facilities. Gage has low-rise apartments and three 17-storey towers that offer spectacular views. A mix of new and returning undergraduate students live here."),
                        h3("Facilities"),
                        p("Meeting Room, Study Space, Lounge, Convenience Store, Waste Sorting Stations, Laundry, Basketball Court, Tennis Court, Park, Medical Clinic"),
                        tags$a(href="https://vancouver.housing.ubc.ca/residences/walter-gage/", 
                               "Click here for more information"),
                        img(src='waltergage.jpeg', align = "center", height=600, width=800)))
             
    ),
    
    navbarMenu("More Insights", 
               tabPanel("External Accommodation",
                        
                        sidebarLayout(
                          sidebarPanel (
                            helpText("Instructions: We have listed some possible Considerations you might have when choosing a place to stay. Feel free to explore the listings according to your needs! "),
                            br(),
                            br(),
                            selectInput(inputId = "c", label = "Considerations", 
                                        choices = c("General Price", "SuperHost", "Supermarket", "Hospital", "Train Station", "Distance from School")),
                            conditionalPanel(condition = "input.c == 'General Price'",
                                             selectInput(inputId = "ptype1", label = "Room Type", choices = unique(listings$room_type)),
                                             radioButtons("superhost1", h3("Is Super Host?"),
                                                          choices = list("Yes" = 1, "No" = 2),selected = 1)
                                             ),
                            conditionalPanel(condition = "input.c == 'SuperHost'",
                                             selectInput(inputId = "ptype2", label = "Room Type", choices = unique(listings$room_type))
                                             ),
                            conditionalPanel(condition = "input.c == 'Supermarket'",
                                             selectInput(inputId = "ptype3", label = "Room Type", choices = unique(listings$room_type)),
                                             radioButtons("superhost2", h3("Is Super Host?"),
                                                          choices = list("Yes" = 1, "No" = 2),selected = 1)
                                             ),
                            conditionalPanel(condition = "input.c == 'Hospital'",
                                             selectInput(inputId = "ptype4", label = "Room Type", choices = unique(listings$room_type)),
                                             radioButtons("superhost3", h3("Is Super Host?"),
                                                          choices = list("Yes" = 1, "No" = 2),selected = 1)
                                             ),
                            conditionalPanel(condition = "input.c == 'Train Station'",
                                             selectInput(inputId = "ptype5", label = "Room Type", choices = unique(listings$room_type)),
                                             radioButtons("superhost4", h3("Is Super Host?"),
                                                          choices = list("Yes" = 1, "No" = 2),selected = 1)
                                             ),

                            conditionalPanel(condition = "input.c == 'Distance from School'",
                                             selectInput(inputId = "ptype7", label = "Room Type", choices = unique(listings$room_type)),
                                             radioButtons("superhost6", h3("Is Super Host?"),
                                                          choices = list("Yes" = 1, "No" = 2),selected = 1)
                                             )
                            ),

                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Charts", withSpinner(plotOutput("ListingAnalysis"))),
                              tabPanel("Maps", withSpinner(leafletOutput("HospitalMap")))
                              )
                            )

                        )),
               tabPanel("Campus Accommodation",
                        
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Instructions: On your right shows various Insights with regards to the Campus Accommodation available in UBC. The filter below would be available under the 'Different Session' tab."),
                            br(),
                            br(),
                            selectInput(inputId = "name", label = "Name of Campus Accommodation", choices = unique(prices$Accom)),
                            br(),
                            img(src = "ubc_pic.jpeg", height=300, width=400),
                            
                          ),
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Price Range", withSpinner(plotOutput("RangeCampus"))),
                              tabPanel("Vacancies", withSpinner(plotOutput("VacancyCampus"))),
                              tabPanel("Different Session", withSpinner(plotOutput("Session")))
                            )
                          )
                        ))  
    )
    
  
))


#server
server <- function(input, output, session) {

  
  #function to filter the dataset for diff category and session
  filterCat <- function() {
    data <- ubc[ubc$Category == input$category & grepl(input$session, ubc$Session), ]
    return(data)
  }
  
  #function to filter external accoms sidebar
  filterHost <- reactive({
    data <- listings[(listings$distance_from_school <= input$distance_from_school & listings$minimum_nights >= input$slider1), ]
    data <- data[data$price >= input$priceRange[1] & data$price <= input$priceRange[2], ]
    if (!is.null(input$neighbourhoods)) {
      data <- data[data$neighbourhood_cleansed %in% input$neighbourhoods, ]
    }
    if (!is.null(input$room_type)) {
      data <- data[data$room_type %in% input$room_type, ]
    }
    return(data)
  })
  
  #function to filter external accoms sidebar
  filterHost <- reactive({
    data <- listings[(listings$distance_from_school <= input$distance_from_school & listings$minimum_nights >= input$slider1), ]
    data <- data[data$price >= input$priceRange[1] & data$price <= input$priceRange[2], ]
    if (!is.null(input$neighbourhoods)) {
      data <- data[data$neighbourhood_cleansed %in% input$neighbourhoods, ]
    }
    if (!is.null(input$room_type)) {
      data <- data[data$room_type %in% input$room_type, ]
    }
    return(data)
  })
  
  #function to filter the reviews for word cloud
  filterReview <- function() {
      
      #filter reviews 
      data <- reviews[reviews$listing_id == input$listing_name, ]
      
      
      x <- data[detect_language(data$comments) == "en", ] %>% 
        mutate(comments = str_replace_all(comments, "\\d+", "")) %>%
        drop_na()
      
      tokens <- x %>% 
        unnest_tokens(word, comments)
      
      # Remove stop words
      stop_words <- data.frame(stopwords("en")) #all the stop words in english
      tokens <- tokens %>%
        anti_join(stop_words, by=c("word" = "stopwords..en.."))
      
      #get the frequency
      word_freq <- tokens %>% filter(!(word %in% c("br", "vancouver"))) %>% group_by(word) %>% summarise(freq = n()) %>% arrange(desc(freq)) %>% filter(freq >= 2)
      
    
      return(word_freq)
  }
  
  #function to filter the selected of accomodation
  filterhome <- function() {
    data <- listings %>% filter(listings$id == input$listing_name)
    return(data)
  }
  #function to filter the nearest hospital
  filterhospital <- function() {
    name <- filterhome()$nearest_hospital
    data1 <- hospitals.data[hospitals.data$facility_name == name,]
    return(data1)
  }
  #function to filter the nearest supermarket
  filtersupermarket <- function() {
    name <- filterhome()$nearest_supermarket
    data2 <- supermarkets.data[supermarkets.data$`Supermarket Name` == name,]
    return(data2)
  }
  #function to filter the nearest community centre
  filtercc <- function() {
    name <- filterhome()$nearest_community_centre
    data3 <- community.centres.data[community.centres.data$NAME == name,]
    return(data3)
  }
    
  #to get the leaflet
  output$MapAccom <- renderLeaflet({
    
    #initiate leaflet
    m <- leaflet() %>% addTiles()
    
    # define the icon for the circle markers
    my_icon <- makeIcon(
      iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-red.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    )
    
    #add layer of location
    m <- addMarkers(m,
                    lng = filterCat()$Lon,
                    lat = filterCat()$Lat,
                    popup = filterCat()$Name,
                    icon = my_icon)
    
    # add a marker for the location of school
    m <- addCircleMarkers(m,
                          lng = -123.2460,
                          lat = 49.2606,
                          popup = "School")
    
    #add map layers
    m <- addTiles(m,group="Default")
    m <- addProviderTiles(m,"Esri.WorldImagery", group = "Esri")
    m <- addProviderTiles(m,"Stamen.Toner", group = "Toner")
    m <- addProviderTiles(m, "Stamen.TonerLite", group = "Toner Lite")
    
    #add controls
    m <- addLayersControl(m, baseGroups = c("Default","Esri",
                                            "Toner Lite","Toner")
    )
    m
  })
  
  
  #to get scatter for the listings according to filters
  output$ScatterListings <- renderLeaflet({
    
    #initiate leaflet
    s <- leaflet() %>% addTiles()
    
    # add a marker for the location accommodations
    s <- addCircleMarkers(s,
                          lng = filterHost()$longitude,
                          lat = filterHost()$latitude,
                          popup = filterHost()$host_id,
                          radius = 2,
                          layerId = filterHost()$id)
    
    # add a marker for the location of school
    s <- addCircleMarkers(s,
                          lng = -123.2460,
                          lat = 49.2606,
                          popup = "School",
                          color = "red")
    
    # add a circle marker around the school with the specified radius
    s <- addCircleMarkers(s,
                          lng = -123.2460,
                          lat = 49.2606,
                          radius = input$distance_from_school/10,
                          fillOpacity = 0.2,
                          color = "red",
                          stroke = TRUE,
                          weight = 1)
    
    # add a marker showing name of listing & price for each listing within the selected distance
    s <- addMarkers(s,
                    lng = filterHost()$longitude,
                    lat = filterHost()$latitude,
                    popup = paste0("<strong>", filterHost()$name, "</strong><br>",
                                   "Listing ID: ", filterHost()$id, "<br>",
                                   "Price: S$", filterHost()$price, "/night"),
                    layerId = filterHost()$id)
    
    
    s
  })
  
  
  observeEvent(input$ScatterListings_marker_click, {
    p <- input$ScatterListings_marker_click
    ggplot_data = as.data.frame(rbind(rep(1,5), rep(5,5)))
    colnames(ggplot_data) = c("response_rating", "amenities_rating", "school_convenience", "convenience", "safety")
    ggplot_data <- rbind(ggplot_data, listings[listings$id == p$id,c("response_rating", "amenities_rating", "school_convenience", "convenience", "safety")])
    
    output$radar = renderPlot({
      radarchart(ggplot_data)
    })
  })
  
  # Nest Count Map
  room_type <- c("Entire home/apt", "Private room", "Hotel room", "Shared room")
  color_factors <- colorFactor(c('Red', 'Green', 'Blue', 'Orange'), domain = room_type)
  
  filtered_listings <- reactive({
    listings %>%
      filter(price >= input$priceRange[1] & price <= input$priceRange[2],
             distance <= input$distance_from_school,
             minimum_nights >= input$slider1,
             neighbourhood_cleansed %in% input$neighbourhoods)
  })

  output$m1 <- renderLeaflet({
    # Define the room types and color factors
    room_type<- c("Entire home/apt","Private room","Hotel room","Shared room")
    colorFactors<-colorFactor(c('Red','Green',"Blue","Orange"), domain =listings$room_type)
    
    
    # Filter the data by room types
    roomtype1<- listings %>% filter(room_type=='Entire home/apt')
    roomtype2<- listings %>% filter(room_type=='Private room')
    roomtype3<- listings %>% filter(room_type=='Hotel room')
    roomtype4<- listings %>% filter(room_type=='Shared room')
    
    # Create a leaflet map
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data=roomtype1, lat =~latitude, lng =~ longitude,popup=paste("Price:", roomtype1$price, "<br>"
                                                                                    , "Name:", roomtype1$name, "-", "<br>"
                                                                                    , "ID", roomtype1$id)
                       , radius =5,color="red",group=room_type[1],clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data=roomtype2, lat =~latitude, lng =~ longitude,popup=paste("Price:", roomtype2$price, "<br>"
                                                                                    , "Name:", roomtype2$name, "-","<br>"
                                                                                    , "ID", roomtype2$id),radius =5,color="green",group=room_type[2],clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data=roomtype3, lat =~latitude, lng =~ longitude,popup=paste("Price:", roomtype3$price, "<br>"
                                                                                    , "Name:", roomtype3$name, "-", "<br>"
                                                                                    , "ID", roomtype3$id) , radius =5,color="blue",group=room_type[3],clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data=roomtype4, lat =~latitude, lng =~ longitude,popup=paste("Price:", roomtype4$price, "<br>"
                                                                                    , "Name:", roomtype4$name, "-", "<br>"
                                                                                    , "ID", roomtype4$id), radius =5,color="orange",group=room_type[4],clusterOptions = markerClusterOptions()) %>%
      addLayersControl(overlayGroups = room_type,options = layersControlOptions(collapsed = FALSE))
  })
  
  #add a wordcloud 
  output$WordCloud <- renderWordcloud2({
    
    wordcloud2(filterReview(), size = 0.8, color = "random-light", backgroundColor = "white", shape = "circle") 
    
  })
  
  #add neighborhood explore next to wordcloud
  output$explore <- renderLeaflet({
    
    #initiate leaflet
    e <- leaflet() %>% addTiles()
    
    # add a marker for the location of hospital
    e <- addCircleMarkers(e,
                          lng = filterhospital()$longitude,
                          lat = filterhospital()$latitude,
                          popup = filterhospital()$facility_name,
                          radius = 10,
                          group = 'Nearest Hospital',
                          color='red')
    # add a marker for the location of supermarket
    e <- addCircleMarkers(e,
                          lng = filtersupermarket()$lon,
                          lat = filtersupermarket()$lat,
                          popup = filtersupermarket()$`Supermarket Name`,
                          radius = 10,
                          group = 'Nearest Supermarket',
                          color='blue')
    # add a marker for the location of community center
    e <- addCircleMarkers(e,
                          lng = filtercc()$lng,
                          lat = filtercc()$lat,
                          popup = filtercc()$NAME,
                          radius = 10,
                          group = 'Nearest Community Centre',
                          color='green')
    
    # add a marker for the location of school
    e <- addMarkers(e,
                    lng = -123.2460,
                    lat = 49.2606,
                    popup = "School")
    # add a marker for the location of home
    e <- addMarkers(e,
                    lng = filterhome()$longitude,
                    lat = filterhome()$latitude,
                    popup = "Your Place")
    e<- addLayersControl(e,
                         overlayGroups = c('Nearest Hospital','Nearest Supermarket','Nearest Community Centre'),
                         options = layersControlOptions(collapsed = FALSE))
    e
  })
  
  
  #plot the listings price
  output$ListingAnalysis <- renderPlot({
    
    if (input$c == "General Price") { 
      
      #filter out the data 
      d <- listings %>% 
        select(neighbourhood_cleansed, price, room_type, host_is_superhost) %>%  
        filter(room_type == input$ptype1 & host_is_superhost == ifelse(input$superhost1 == 1, T, F)) %>% #is it correct?
        group_by(neighbourhood_cleansed) %>% 
        summarise(avg_price = mean(price, na.rm=TRUE)) %>%
        arrange(avg_price)
      
      ggplot(d, aes(y = reorder(neighbourhood_cleansed, avg_price), x = avg_price)) +
        geom_point(size = 4, color = "darkred") + # Add the points
        geom_segment(aes(xend = 0, yend = neighbourhood_cleansed),
                     linewidth = 1.5, color = "darkred") +
        geom_text(aes(label = paste0("$", round(avg_price))),
                  hjust = -0.4, size = 3.5, color = "black") +
        theme_minimal() + 
        labs(title = "Average Airbnb Listing Price by Neighbourhood",
             x = "Average Price", y = "Neighbourhood") +
        theme(plot.title = element_text(hjust = 0.5)) 
      
    } else if (input$c == "SuperHost") {
      
      superhost_avg <- listings %>%
        filter(room_type == input$ptype2) %>%
        group_by(neighbourhood_cleansed) %>%
        summarise(prop_superhosts = mean(host_is_superhost)) %>%
        arrange(prop_superhosts) %>%
        mutate(neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = unique(reorder(neighbourhood_cleansed, prop_superhosts))))
      
      
      ggplot(superhost_avg, aes(x = prop_superhosts, y = neighbourhood_cleansed)) +
        geom_segment(aes(x = 0, xend = prop_superhosts, y = neighbourhood_cleansed, yend = neighbourhood_cleansed), color = "darkred", size = 1.5) +
        geom_point(aes(x = prop_superhosts, y = neighbourhood_cleansed), size = 4, color = "darkred", fill = "white") +
        geom_text(aes(x=prop_superhosts + 0.04, y=neighbourhood_cleansed, 
                      label=paste0(round(prop_superhosts*100,1),"%")), 
                  hjust=0, size=3.5) +
        labs(title = "Percentage of Superhosts per Neighbourhood", y = "Neighbourhood", x = "Percentage of Superhosts(%)") +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05)) +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 8)) +
        theme(plot.title = element_text(hjust = 0.5)) 
      
      
    } else if (input$c == "Supermarket") {
      
      #number of supermarkets and its distance
      market <- listings %>% 
        select(neighbourhood_cleansed, supermarkets, nearest_supermarket_dist, room_type, host_is_superhost) %>% 
        filter(room_type == input$ptype3 & host_is_superhost == ifelse(input$superhost2 == 1, T, F)) %>% 
        group_by(neighbourhood_cleansed) %>%
        summarise(avg_supermarkets = mean(supermarkets),
                  avg_supermarket_dist = mean(nearest_supermarket_dist))
      
      #create the ggplot object and add two y-axes
      ggplot(data = market, aes(x = neighbourhood_cleansed)) +
        geom_col(aes(y = round(avg_supermarkets), fill = "Number of Supermarkets")) + #num of supermarkets being rounded up 
        geom_line(aes(y = avg_supermarket_dist/1000, group = 1, color = "Distance to the Nearest Supermarket")) +
        scale_y_continuous(name = "Number of Supermarkets", sec.axis = sec_axis(~.*1000, name = "Distance (m)")) +
        scale_fill_manual(values = "lightblue", name = NULL) +
        scale_color_manual(values = "darkred", name = NULL) +
        labs(title = "Supermarkets by Neighbourhood",
             x = "Neighbourhood",
             y = "Number of Supermarkets") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(plot.title = element_text(hjust = 0.5)) 
      
      
    } else if (input$c == "Hospital") {
      
      hospital <- listings %>% 
        select(neighbourhood_cleansed, nearest_community_centre_dist, room_type, host_is_superhost) %>%
        filter(room_type == input$ptype4 & host_is_superhost == ifelse(input$superhost3 == 1, T, F)) %>%
        group_by(neighbourhood_cleansed) %>%
        summarise(avg_cc_dist = mean(nearest_community_centre_dist))
      
      ggplot(data = hospital, aes(x = neighbourhood_cleansed)) +
        geom_line(aes(y = avg_cc_dist, group = 1, colour = "darkred")) +
        labs(title = "Nearest Hospital by Neighbourhood",
             x = "Neighbourhood",
             y = "Distance (m)") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        guides(colour = "none") +
        theme(plot.title = element_text(hjust = 0.5)) 
      
      
    } else if (input$c == "Train Station") {
      
      train <- listings %>% 
        select(neighbourhood_cleansed, nearest_train_station_dist, room_type, host_is_superhost) %>%
        filter(room_type == input$ptype5 & host_is_superhost == ifelse(input$superhost4 == 1, T, F)) %>%
        group_by(neighbourhood_cleansed) %>%
        summarise(avg_t_dist = mean(nearest_train_station_dist))
      
      ggplot(data = train, aes(x = neighbourhood_cleansed)) +
        geom_line(aes(y = avg_t_dist, group = 1, colour = "darkred")) +
        labs(title = "Nearest Train Station by Neighbourhood",
             x = "Neighbourhood",
             y = "Distance (m)") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        guides(colour = "none") +
        theme(plot.title = element_text(hjust = 0.5)) 
     
    } else if (input$c == "Distance from School") {
      
      dist <- listings %>%
        filter(room_type == input$ptype7 & host_is_superhost == ifelse(input$superhost6 == 1, T, F))
      
      ggplot(dist, aes(x=distance_from_school, y=price))+
        geom_point(colour = "lightblue") +
        labs(title = "Relationship between Distance and Price",
             x = "Distance from school (m)", 
             y = "Price of Property") +
        theme(plot.title = element_text(hjust = 0.5)) 
      
    }
  
    
  })
  
  
  #to get the price range
  output$RangeCampus <- renderPlot({
    
    ggplot(ubc) +
      geom_segment( aes(x=Name, xend=Name, y=Min.Monthly.Price, yend=Max.Monthly.Price), color="black") +
      geom_point( aes(x=Name, y=Min.Monthly.Price), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
      geom_point( aes(x=Name, y=Max.Monthly.Price), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
      coord_flip() +
      theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      labs(title = "Prices for Different Campus Accommodation",
           x = "Accommodation Names",
           y = "Prices") +
      theme(plot.title = element_text(hjust = 0.5)) 
    
  })
  
  
  #for the vacancies and bedroom 
  output$VacancyCampus <- renderPlot({
    
    ggplot(ubc) +
      geom_point(aes(x=Num.Of.Beds, y=Average.Price.Per.Night)) +
      geom_smooth(aes(x=Num.Of.Beds, y=Average.Price.Per.Night)) +
      labs(title = "Relationship between Vacancies and Average Price Per Night",
           y = "Average Price Per Night",
           x = "Number of Beds Available") +
      theme(plot.title = element_text(hjust = 0.5)) 
    
    
  })
  
  output$HospitalMap <- renderLeaflet({
    
    if (input$c == "Hospital") {
      
      hospital_type<- c("A&E","No A&E")
      colorFactors2<-colorFactor(c('Red',"Blue"), domain = hospitals.data$`A&E Facility`)
      hospitaltype1<- hospitals.data %>% filter(`A&E Facility` == "Yes")
      hospitaltype2<- hospitals.data %>% filter(`A&E Facility` == "No")
      
      m2<-leaflet(hospitals.data) %>% 
        addTiles() %>% 
        addCircleMarkers(data=hospitaltype1, lat =~latitude, lng =~ longitude,popup=paste("Name:", hospitals.data$facility_name,"<br>", "Address",hospitals.data$source_format_str_address, "<br>",
                                                                                          "A&E Facility: ", ifelse(hospitals.data$`A&E Facility`, "Yes", "No"))
                         , radius =5,color="red", group = hospital_type[1],clusterOptions = markerClusterOptions()) %>%
        addCircleMarkers(data=hospitaltype2, lat =~latitude, lng =~ longitude,popup=paste("Name:", hospitals.data$facility_name,"<br>", "Address",hospitals.data$source_format_str_address, "<br>",
                                                                                          "A&E Facility: ", ifelse(hospitals.data$`A&E Facility`, "Yes", "No")),radius =5,color="green", group = hospital_type[2],clusterOptions = markerClusterOptions())%>%
        addLayersControl(overlayGroups = hospital_type,options = layersControlOptions(collapsed = FALSE)) 
      
      m2
    }
    
  })
  
  output$Session <- renderPlot({
    
    test2 <- prices[prices$Accom == input$name,]
    ggplot(test2, aes(x=Room.Type, y=Total, fill=Type)) +
      geom_col(position="dodge", width=0.8) +
      labs(title = "Prices According to Different Session",
           x = "Room Type",
           y = "Price per Month") +
      coord_flip()
    
  })
  
}
  

#run app
shinyApp(ui = ui, server = server)
