
content <- paste(sep = "<br/>",
                 paste("<b>",top3()$Neighborhood[i],"</b>", sep = ""),
                 paste("Parks:", top3()$park.num[i],"  Healthcare:", top3()$heal.num[i]),
                 paste("Library:", top3()$lib.num[i],"  Restaurant:", top3()$rest.num[i]),
                 paste("The neighborhood is...umm...", top3()$density[i])
)

output$mymap <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    addPopups(-122.327298, 47.597131, content,
              options = popupOptions(closeButton = FALSE)
    )
})