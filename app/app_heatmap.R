
library(shiny)
library(ggplot2)
library(ggmap)
library(choroplethrZip)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Perfect City Go", theme="black.css",
                         
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
                                          plotOutput("heatmap")
                                      )
                                  )))
              
              
)

server <- shinyServer(function(input, output){
    data("zip.regions")
    map <- reactive({
        if (input$asp == 1){
            d <- read.csv("~/GitHub/Spr2017-proj2-proj2_grp8/data/All-in/Population.csv")
            d <- d[d$city == input$city,]
            d <- na.omit(d)
            d_map <- get_map(location = input$city ,maptype = "terrain" , zoom = 12)
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
    
    
    
    
    
    output$heatmap <- renderPlot({
            map()
        
        
    }
    
        
    )
})
shinyApp(ui=ui, server = server)
