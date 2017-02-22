library(shiny)


main <- read.csv("../data/City Data/main.csv", as.is = TRUE)

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
                                               choices = c("New York City" =  "NYC",
                                                           "Los Angeles" = "LA",
                                                           "San Francisco" = "SF",
                                                           "Austin" = "Austin",
                                                           "Chicago" = "Chicago")
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
                                   tableOutput("view")
                                 )
                               )))
           
           
)

server=shinyServer(function(input, output){})
shinyApp(ui=ui, server = server)