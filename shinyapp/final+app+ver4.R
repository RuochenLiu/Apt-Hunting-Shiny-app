library(shiny)
library(shinythemes)

library(ggplot2)
library(ggmap)
library(choroplethrZip)
library(plyr)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)
require(RColorBrewer)
library(leaflet)
library(zipcode)
suppressPackageStartupMessages(library(tilegramsR))
setwd("data")

downloaddir<-getwd()


main <- read.csv("data/City Data/main.csv", as.is = TRUE)
data(zipcode)
source("source/compare.R")
source("source/Score.R")
nb <- read.csv("data/City Data/NBHD.csv")
nb$NB <- as.character(nb$NB)



ui=shinyUI(
  div(id='canvas',
      navbarPage(strong('Perfect City Go', style='color:black;'), theme = shinytheme('darkly'),
                 
                 ### 1. INTRODUCTION TAB
                 tabPanel('Introduction',
                          mainPanel(width=12,
                                    h2('a RShiny app to choose your perfect city in USA'),
                                    br(),
                                    h3('Background'),
                                    p('We notice USA is becoming more and more popular for people all around the word,
                                      especially for several famous cities such as New York, Chicago, Austin, Los Angeles and San Francisco.
                                      These cities are all such big apples to give you convenience of living and job opportunities.
                                      Chooing the most suitable place for you seems a hot topic.'),
                                    br(),
                                    h3('Summary of this APP'),
                                    p('-',strong('City Description'),':presents 4 visualizations for job opportunities both for different types and differnet cities, wages differences, population and facilities.'),
                                    p('-',strong('Heat Map'),':shows different heatmap in 5 cities when considering several factors such as restaurant and rent.'),
                                    p('-',strong('Find Your Place'), ':enables users to pinpoint the most importatn factors to finally search the most suitable place with map.'),
                                    p('-',strong('Contact'),':shows group members contact for further questions.'),
                                    br(),
                                    h3('Quick Start'),
                                    p('1. Get yourself familar with the results showing in the City Description tab.'),
                                    p('2. Choose your expected city with different factors in Heatmap tab. '),
                                    p('3. Choose your expected rent range and type, population density and factors to weigh in Find Your Place tab. '),
                                    p('4. Get your result showing the map.'),
                                    p('5. Enjoy!'),
                                    br(),
                                    p(em(a("Github Link",href='https://github.com/TZstatsADS/Spring2017-Proj2-grp8'))),
                                    div(class='footer','Applied Data Science')
                                    
                                    )),
                 
                 # heatmap TAB
                 tabPanel('heatmap',
                          titlePanel(
                              h2("heatmap of your city"
                              )),
                          
                          sidebarLayout(
                              sidebarPanel(
                                  fixed=TRUE,draggable=TRUE,
                                  top=60,left="auto",right=20,bottom="auto",
                                  width=330,height="auto",
                                  
                                  selectInput("city", 
                                              label = "Where are you living?",
                                              choices = c("New York" = "New York" ,
                                                          "Los Angeles" = "Los Angeles",
                                                          "San Francisco" = "San Francisco",
                                                          "Austin" = "Austin",
                                                          "Chicago" = "Chicago")
                                  ),
                                  selectInput("asp", 
                                              label = "Which aspect you want to learn about?",
                                              choices = c("Population" =  1,
                                                          "crime rate" = 2,
                                                          "library" = 3,
                                                          "Restaurant" = 4,
                                                          "park" = 5,
                                                          "health care" =6)
                                  )
                              ),
                              
                              
                              mainPanel(
                                  plotOutput("heatmap",width = "800px" , height = "800px")
                              )
                          )),
                 
                 # 4. FIND YOUR NEIGHBORHOOD TAB
                 tabPanel('Find Your Neighborhood',
                          titlePanel(
                              h2("Reallocating to a new city?", 
                                 br(), 
                                 "Find the neighborhood that's perfect for you")),
                          
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  
                                  selectInput("target_city", 
                                              label = "Where are you reallocating to?",
                                              choices = c("New York City" =  "ny",
                                                          "Los Angeles" = "la",
                                                          "San Francisco" = "sfo",
                                                          "Austin" = "aus",
                                                          "Chicago" = "chi")
                                  ),
                                  br(),
                                  p(h4(strong("Happy with your current neighborhood? Let's find a similar one in your target city"))),
                                  selectInput("current_city", 
                                              label = "Current city",
                                              choices = c("Not Selected" = "NA",
                                                          "New York City" = "NY",
                                                          "Los Angeles" = "LA",
                                                          "San Francisco" = "SF",
                                                          "Austin" = "Austin",
                                                          "Chicago" = "Chicago")
                                  ),
                                  br(),
                                  selectInput("current_neighborhood", 
                                              label = "Current neighborhood",
                                              #choices = c("Not Selected" = "NA", "UW" = "Upper West Side")
                                              choices = c("Not Selected" = "NA", main[,2])
                                  ),
                                  br(),
                                  selectInput("current_br", 
                                              label = "Number of bedrooms of your current residence",
                                              choices = c("Studio" = 0,
                                                          "1b" = 1,
                                                          "2b" = 2,
                                                          "3b" = 3,
                                                          "4b" = 4)
                                  ),
                                  br(),
                                  p(h4(strong("Or, adjust your criteria manually"))),
                                  selectInput("manual_br", 
                                              label = "Number of bedrooms",
                                              choices = c("Studio" = 0,
                                                          "1b" = 1,
                                                          "2b" = 2,
                                                          "3b" = 3,
                                                          "4b" = 4)
                                  ),
                                  br(),
                                  sliderInput("manual_rent", 
                                              label = "Rent range", 
                                              min = 850, 
                                              max = 7900, 
                                              value = c(1200, 2000), 
                                              step = 10, 
                                              round = TRUE),
                                  br(),
                                  selectInput("manual_density", 
                                              label = "Population density",
                                              choices = c("Crowded" = 3,
                                                          "Medium" = 2,
                                                          "Sparse" = 1)
                                  ),
                                  br(),
                                  p(h4(strong("Other factors to weigh in?"))),
                                  checkboxInput("health", 
                                                label = "Healthcare facilities", 
                                                value = FALSE),
                                  checkboxInput("libraries", 
                                                label = "Libraries", 
                                                value = FALSE),
                                  checkboxInput("parks", 
                                                label = "Parks", 
                                                value = FALSE),
                                  checkboxInput("restaurants", 
                                                label = "Restaurants", 
                                                value = FALSE)
                              ),
                              
                              
                              mainPanel(
                                  tableOutput("view"),
                                  tabsetPanel(type="pill",id="inTabset",
                                              tabPanel("leafletmap",
                                                       leafletOutput("leafletmap",height =500)
                                              ),
                                              tabPanel("streetmap",
                                                       leafletOutput("streetmap",height=500)
                                              )
                                             
                                              
                                  )
                              )
                          )),
      
                 
                 ### 5.CONTACT TAB
                 tabPanel('Contact',
                          mainPanel(width=12,
                                    h2('Contact Information'),
                                    br(),
                                    p('We are Columbia university students at Department of statistics and Actuarial Science.'),
                                    p('If you are interested in our project or have questions about APP, feel free to contact us.'),
                                    br(),
                                    h4('Our email address are as follows:'),
                                    br(),
                                    p(strong('Ruochen Liu'),':rl2841@columbia.edu'),
                                    p(strong('Bo Peng'),':bp2494@columbia.edu'),
                                    p(strong('Zheren Tang'),':zt2191@columbia.edu'),
                                    p(strong('Mengchen Li'),':ml3890@columbia.edu'),
                                    p(strong('Yuan Mei'),':ym2583@columbia.edu'),
                                    br(),
                                    p(em(a("Github Link",href='https://github.com/TZstatsADS/Spring2017-Proj2-grp8')))
                                    
                          ))
                 
      ))
)

server=shinyServer(function(input, output,session){
    data("zip.regions")
    map <- reactive({
        if (input$asp == 1){
            d <- read.csv("data/All-in/Population.csv")
            d <- d[d$city == input$city,]
            d <- na.omit(d)
            d_map <- get_map(location = input$city ,maptype = "terrain" , zoom = 13)
            map <- ggmap(d_map, extent = "device") + 
                geom_density2d(data = d, 
                               
                               aes(x = Lon, y = Lat), size = 0.3) + 
                stat_density2d(data = d, 
                               
                               aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
                scale_alpha(range = c(0, 0.3), guide = FALSE)
            
            
        }
        
        if(input$asp == 2 ){
            crime <- read.csv(paste("data/crime/",input$city,".csv",sep = ""))
            crime <- na.omit(crime)
            crime_map <- get_map(location = input$city,maptype = 'terrain',zoom = 12)
            map <- ggmap(crime_map, extent = "device") + 
                geom_density2d(data = crime, 
                               
                               aes(x = Lon, y = Lat), size = 0.3) + 
                stat_density2d(data = crime, 
                               
                               aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
                scale_alpha(range = c(0, 0.3), guide = FALSE)
        }
        
        if(input$asp == 3){
            lib <- read.csv(paste("data/City Raw/",input$city, "/Library.csv", sep = ""))
            lib=
                lib%>%
                filter(lib$ZIP>0)%>%
                mutate(region=as.character(ZIP))
            lib=
                lib%>%
                group_by(region)%>%
                summarise(
                    value=n()
                )
            map <- zip_choropleth(lib,
                                  title       = paste("Library in ", input$city, sep = ""),
                                  legend      = "Number of Libraries",
                                  zip_zoom = lib$region)
            
        }
        if (input$asp == 4){
            res <- read.csv(paste("dat/aCity Raw/",input$city, "/Restaurant.csv", sep = ""))
            res=
                res%>%
                filter(res$ZIP>0)%>%
                mutate(region=as.character(ZIP))
            res=
                res%>%
                group_by(region)%>%
                summarise(
                    value=n()
                )
            map <- zip_choropleth(res,
                                  title       = paste("Restaurant in ", input$city, sep = ""),
                                  legend      = "Number of restaurants",
                                  zip_zoom = res$region[res$region %in% zip.regions$region])
        }
        if (input$asp == 5){
            res <- read.csv(paste("data/City Raw/",input$city, "/Park.csv", sep = ""))
            res=
                res%>%
                filter(res$ZIP>0)%>%
                mutate(region=as.character(ZIP))
            res=
                res%>%
                group_by(region)%>%
                summarise(
                    value=n()
                )
            map <- zip_choropleth(res,
                                  title       = paste("Park in ", input$city, sep = ""),
                                  legend      = "Number of Parks",
                                  zip_zoom = res$region[res$region %in% zip.regions$region])
        }
        if (input$asp == 6){
            res <- read.csv(paste("data/City Raw/",input$city, "/Health.csv", sep = ""))
            res=
                res%>%
                filter(res$ZIP>0)%>%
                mutate(region=as.character(ZIP))
            res=
                res%>%
                group_by(region)%>%
                summarise(
                    value=n()
                )
            map <- zip_choropleth(res,
                                  title       = paste("Health care in ", input$city, sep = ""),
                                  legend      = "Number of Health cares",
                                  zip_zoom = res$region[res$region %in% zip.regions$region])
        }
        
        
        return(map)
    })
    
    
    
    
    
    output$heatmap <- renderPlot( {
        map()
        
        
    }
    
    
    )

    
    # Get target city.
    
    top3 <- reactive({
        city <- main[main$City == input$target_city,]
        
        # If user chooses his current neighbirhood.
        
        if(input$current_neighborhood != "NA"){
            st <- main[main$Neighborhood == input$current_neighborhood, ]
            top <- comp(city, st[1,], as.numeric(input$current_br))
        }
        
        # If not
        
        else{
            
            br <- as.numeric(input$manual_br) + 9
            up <- as.numeric(input$manual_rent[2])
            down <- as.numeric(input$manual_rent[1])
            den <- as.numeric(input$manual_density)
            if(den == 3){
                den0 <- 150
                den1 <- 500
            }
            if(den == 2){
                den0 <- 50
                den1 <- 150
            }
            if(den == 1){
                den0 <- 0
                den1 <- 50
            }
            
            # Check rent range.
            
            dest <- city[city[,br] <= up & city[,br] >= down & city$density < den1 & city$density >den0, ]
            
            
            if(nrow(dest) == 0){
                top <- NA
            }
            else {
                if(nrow(dest) < 3 & nrow(dest) >0){
                    top <- dest
                }
                else{
                    p <- as.numeric(input$parks)
                    r <- as.numeric(input$restaurants)
                    h <- as.numeric(input$health)
                    l <- as.numeric(input$libraries)
                    top <- score(dest, p, h, l, r) 
                }
            }
        }
        
        
        
        if(is.na(top) == FALSE){
            if(top$density[1] >150){
                top$density <- rep("Crowded", nrow(top))
            }
            else{
                if(top$density[1] < 50){
                    top$density <- rep("Sparse", nrow(top))
                }
                else{
                    top$density <- rep("Medium", nrow(top))
                }
            }
        }
        
        if(is.na(top) == FALSE){
            colnames(top) <- c("City","Neighborhood","Park","Healthcare","Library","Restaurant","Population","Area","Rent-Studio","Rent-1BR","Rent-2BR","Rent-3BR","Rent-4BR","Pop-Density")
        }
        
        return(top)
        
    })
    
    
    
    zip_code <- reactive({
        
        if(is.na(top3()) == FALSE){
            
            #ZipCode List
            zip1 <- strsplit(as.character(nb$ZipCode[nb$NB == top3()$Neighborhood[1]]), ", ")
            zip2 <- strsplit(as.character(nb$ZipCode[nb$NB == top3()$Neighborhood[2]]), ", ")
            zip3 <- strsplit(as.character(nb$ZipCode[nb$NB == top3()$Neighborhood[3]]), ", ")
            
            if(length(zip1) == 1){
                zip1 <- zip1[[1]]
            }
            else{
                zip1 <- NA
            }
            if(length(zip2) == 1){
                zip2 <- zip2[[1]]
            }
            else{
                zip2 <- NA
            }
            if(length(zip3) == 1){
                zip3 <- zip3[[1]]
            }
            else{
                zip3 <- NA
            }
            
            z <- list(zip1, zip2, zip3)
        }
        
        else{
            zip1 <- NA
            zip2 <- NA
            zip3 <- NA
            z <- list(zip1,zip2,zip3)
        }
        
        return(z) # Zip Code List, Length=3
    })
    
    N <- "Sorry! There is no result." 
    
    
    
    # Table Results
    
    output$view <- renderTable({
        if(is.na(top3()) == FALSE){
            head(top3())
        }
        else{
            head(N)
        }
    })
    
    #get zipcode latitude
    data(zipcode)
    trans_ziplat<-function(zip,data=zipcode){
      zip=zip[!is.na(zip)]
      num<-length(zip)
      lat<-NULL
      lng<-NULL
      result=NULL
      if(!length(zip)==0){
      for(i in 1:num){
        lat[i]<-data[which(data$zip==zip[[i]][1]),4]
        lng[i]<-data[which(data$zip==zip[[i]][1]),5]
        }
        result=data.frame(Lat=lat,Lng=lng)
        }
      else{result=NA}
      return(result)
    }
    
    # Leaflet map
    
    crsmerc<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    downloaddir<-getwd()
    leafletmap <- reactive({
        
        c <- paste('zipcode_',input$target_city, sep = "")
        ny_dat <-readOGR(downloaddir, c) 
        
        ny_transformed<-spTransform(ny_dat,CRS=crsmerc)
        
        pattern <- "zip$|ZIPCODE$|zipcode$"
        q <- grep(pattern, names(ny_transformed))
        names(ny_transformed)[q] <- "ZIPCODE"
        
        highlight_3 <- subset(ny_transformed, ny_transformed$ZIPCODE %in% factor(zip_code()[[3]]))
        highlight_2 <- subset(ny_transformed, ny_transformed$ZIPCODE %in% factor(zip_code()[[2]]))
        highlight_1 <- subset(ny_transformed, ny_transformed$ZIPCODE %in% factor(zip_code()[[1]]))
        c_label=ny_transformed$ZIPCODE
        label_1<-top3()$Neighborhood[1]
        label_2<-top3()$Neighborhood[2]
        label_3<-top3()$Neighborhood[3]
        
       if(!is.na(trans_ziplat(zip=zip_code()))){
         ny_map<- leaflet(ny_transformed) %>%
            addTiles(options=providerTileOptions(noWrap=T))%>%
           addProviderTiles("Stamen.Watercolor")%>%
           addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#ffffb2",highlightOptions = highlightOptions(color="#006d2c",opacity = 0.5, weight = 2, fillOpacity = 0.5,
                                                                                                                        bringToFront = TRUE, sendToBack = TRUE),label=c_label) %>%
           addPolygons(data = highlight_3, options = pathOptions(),opacity=0.5,weight=1,color="#006d2c",label=label_3)%>%
           addPolygons(data = highlight_2, options = pathOptions(),opacity=0.5,weight=1,color="#253494",label=label_2)%>%
           addPolygons(data = highlight_1, options = pathOptions(),opacity=0.5,weight=1,color="#cb181d",label=label_1)%>%
           setView(lat=trans_ziplat(zip=zip_code())$Lat[1],lng=trans_ziplat(zip=zip_code())$Lng[1],zoom=11)%>%
           addMarkers(lat=trans_ziplat(zip=zip_code())$Lat,trans_ziplat(zip=zip_code())$Lng)
        

        return(ny_map)
       }
        else{
          ny_map<- leaflet(ny_transformed) %>%
            addTiles(options=providerTileOptions(noWrap=T))%>%
            addProviderTiles("Stamen.Watercolor")%>%
            addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#ffffb2",highlightOptions = highlightOptions(color="#006d2c",opacity = 0.5, weight = 2, fillOpacity = 0.5,
                                                                                                                         bringToFront = TRUE, sendToBack = TRUE),label=c_label)
          return(ny_map)
        }
    })
    
    output$leafletmap <- renderLeaflet({
        leafletmap()
    })
    
    
    streetmap<-reactive({
      streetmap<-leaflet()%>%
        addTiles(options=providerTileOptions(noWrap=TRUE))
      return(streetmap)
      })
    output$streetmap<-renderLeaflet({
      streetmap()
    })
    
    observeEvent(input$leafletmap_shape_click,{
      click<-input$leafletmap_shape_click
      updateTabsetPanel(session, inputId="inTabset", selected ="streetmap" )
      output$streetmap<-renderLeaflet({
        leaflet(streetmap)%>%
          addTiles(options=providerTileOptions(noWrap=TRUE))%>%
         setView(lng = click$lng, lat = click$lat, zoom = 16)
    })
    })
    
})

shinyApp(ui=ui, server = server)
