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
                                    p(em('Github link',href='https://github.com/TZstatsADS/Spring2017-Proj2-grp8')),
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
                                    p(em('Github link',href='https://github.com/TZstatsADS/Spring2017-Proj2-grp8'))
                                    
                          ))
                 
      ))
)

server=shinyServer(function(input, output){
    data("zip.regions")
    map <- reactive({
        if (input$asp == 1){
            d <- read.csv("~/GitHub/Spr2017-proj2-proj2_grp8/data/All-in/Population.csv")
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
            crime <- read.csv(paste("~/GitHub/Spr2017-proj2-proj2_grp8/data/crime/",input$city,".csv",sep = ""))
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
            lib <- read.csv(paste("~/GitHub/Spr2017-proj2-proj2_grp8/data/City Raw/",input$city, "/Library.csv", sep = ""))
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
            res <- read.csv(paste("~/GitHub/Spr2017-proj2-proj2_grp8/data/City Raw/",input$city, "/Restaurant.csv", sep = ""))
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
            res <- read.csv(paste("~/GitHub/Spr2017-proj2-proj2_grp8/data/City Raw/",input$city, "/Park.csv", sep = ""))
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
            res <- read.csv(paste("~/GitHub/Spr2017-proj2-proj2_grp8/data/City Raw/",input$city, "/Health.csv", sep = ""))
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
})

shinyApp(ui=ui, server = server)
