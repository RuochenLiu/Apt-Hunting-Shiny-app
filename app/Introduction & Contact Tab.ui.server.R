library(shiny)
library(shinythemes)

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

server=shinyServer(function(input, output){})

shinyApp(ui=ui, server = server)
