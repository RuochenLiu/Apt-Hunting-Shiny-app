library(shiny)

setwd('C:/Users/Lloyd/Documents/GitHub/Spr2017-proj2-grp8/app')
main <- read.csv("../data/City Data/main.csv")

ui=shinyUI(navbarPage("Perfect City Go", theme="black.css",
                      
                      # 4. FIND YOUR NEIGHBORHOOD TAB
                      tabPanel('Find Your Neighborhood',
                               titlePanel(
                                 h2("Reallocating to a new city?", 
                                    br(), 
                                    "Find the neighborhood that's perfect for you")),
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   fixed=TRUE,draggable=TRUE,
                                   top=60,left="auto",right=20,bottom="auto",
                                   width=330,height="auto",
                                   
                                   selectInput("target_city", 
                                               label = "Where are you reallocating to?",
                                               choices = c("New York City" =  1,
                                                           "Los Angeles" = 2,
                                                           "San Francisco" = 3,
                                                           "Austin" = 4,
                                                           "Chicago" = 5)
                                               ),
                                   br(),
                                   p(h4(strong("Happy with your current neighborhood? Let's find a similar one in your target city"))),
                                   selectInput("current_city", 
                                               label = "Current city",
                                               choices = c("New York City",
                                                           "Los Angeles",
                                                           "San Francisco",
                                                           "Austin",
                                                           "Chicago")
                                               ),
                                   br(),
                                   selectInput("current_neighborhood", 
                                               label = "Current neighborhood",
                                               choices = c("1","2")
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
                                               choices = c("Crowded",
                                                           "Medium",
                                                           "Sparse")
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
                                 
                                 
                                 mainPanel()
                                 )))
                      
                      
)

server=shinyServer(function(input, output){})
shinyApp(ui=ui, server = server)