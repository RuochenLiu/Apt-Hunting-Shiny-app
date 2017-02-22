
library(shiny)



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
                                                      choices = c("New York City" =  1,
                                                                  "Los Angeles" = 2,
                                                                  "San Francisco" = 3,
                                                                  "Austin" = 4,
                                                                  "Chicago" = 5)
                                                    ),
                                          selectInput("asp", 
                                                      label = "Which aspect you want to learn about?",
                                                      choices = c("Population" =  1,
                                                                  "crime rate" = 2,
                                                                  "public facilities" = 3,
                                                                  "restaurant" = 4)
                                          )
                                             ),
                                      
                                      
                                      mainPanel()
                                  )))
              
              
)

server <- shinyServer(function(input, output){})
shinyApp(ui=ui, server = server)
