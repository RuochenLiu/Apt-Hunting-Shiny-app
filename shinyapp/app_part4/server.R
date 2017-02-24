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
  
  
})
