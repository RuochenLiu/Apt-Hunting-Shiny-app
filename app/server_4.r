library(shiny)
library(choroplethr)
library(choroplethrZip)
library(dplyr)

source("../doc/compare.R")
source("../doc/score.R")
source("../doc/crowded.R")
nb <- read.csv("../data/City Data/NBHD.csv")
main <- read.csv("../data/City Data/Main.csv")
#main <- crowd(main)
nb$NB <- as.character(nb$NB)


# Define server to find top three neighborhood for users.
shinyServer(function(input, output) {
  
  # Get target city.
  
  top3 <- reactive({
    city <- main[main$City == input$target_city,]
    
    # If user chooses his current neighbirhood.
    
    if(input$current_city != "NA"){
      st <- main[main$Neighborhood == input$current_neighborhood, ]
      top <- comp(city, st[1,], as.numeric(input$current_br))
    }
    
    # If not
    
    else{

      br <- as.numeric(input$manual_br) + 9
      up <- as.numeric(input$manual_rent[2])
      down <- as.numeric(input$manual_rent[1])
      den <- as.numeric(input$manual_density)
      
      # Check rent range.
      
      dest <- city[city[,br] <= up & city[,br] >= down, ]
      
      if(nrow(dest) <= 3 & nrow(dest) >0){
        top <- dest
      }
      
      if(nrow(dest) == 0){
        top <- NA
      }
      
      else{
        p <- as.numeric(input$parks)
        r <- as.numeric(input$restaurants)
        h <- as.numeric(input$health)
        l <- as.numeric(input$libraries)
        top <- score(dest, p, h, l, r) 
      }
      
    }
   
    
    #ZipCode List
    zip1 <- strsplit(as.character(nb$ZipCode[nb$NB == top$Neighborhood[1]]), ", ")
    zip2 <- strsplit(as.character(nb$ZipCode[nb$NB == top$Neighborhood[2]]), ", ")
    zip3 <- strsplit(as.character(nb$ZipCode[nb$NB == top$Neighborhood[3]]), ", ")
    
    zipcode <- list(zip1[[1]], zip2[[1]], zip3[[1]])
    
    
    return(top)
  })
  
  
  
  
  # Results
  
  
  output$view <- renderTable({
    head(top3())
  })
  
  
})
