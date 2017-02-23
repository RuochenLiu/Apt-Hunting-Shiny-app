library(shiny)
library(choroplethr)
library(choroplethrZip)
library(dplyr)

source("../doc/compare.R")
source("../doc/Score.R")
nb <- read.csv("../data/City Data/NBHD.csv")
main <- read.csv("../data/City Data/Main.csv")
nb$NB <- as.character(nb$NB)


# Define server to find top three neighborhood for users.
shinyServer(function(input, output) {
  
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
    
    return(top)
  })
  
  
  
  zip_code <- reactive({
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
    
    z <- list(zip1[[1]], zip2[[1]], zip3[[1]])
    
    
    return(z) # Zip Code List, Length=3
  })
  
 
  
  
  
  # Results
  
  output$view <- renderTable({
    head(top3())
  })
  
  
})
