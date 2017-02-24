#common
library(rgdal)
library(rgeos)
library(sp)
require(RColorBrewer)
library(leaflet)
suppressPackageStartupMessages(library(tilegramsR))
library(ggmap)
downloaddir<-getwd()
zip_code <- list("10025", "10024")
#city map by zipcode
#ny
ny_dat <-readOGR(downloaddir, "zipcode_ny") 
crsmerc<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
ny_transformed<-spTransform(ny_dat,CRS=crsmerc)
ny_label<-paste(ny_transformed$ZIPCODE,"in",ny_transformed$PO_NAME)
highlight_1 <- subset(ny_transformed, ny_transformed$ZIPCODE %in% factor(zip_code[[1]]))
ny_map<-leaflet(ny_transformed) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#660000",highlightOptions = highlightOptions(color="#FFCC99",opacity = 0.5, weight = 2, fillOpacity = 0.5,
              bringToFront = TRUE, sendToBack = TRUE),label=ny_label,labelOptions = labelOptions(clickable=T,style = list(
                "color" = "black",
                "font-family" = "serif",
                "font-style" = "italic",
                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                "font-size" = "12px",
                "border-color" = "rgba(0,0,0,0.5)"))) %>%
  addPolygons(data = highlight_1, options = pathOptions(),opacity=0.8,weight=1,color="#e34a33",highlightOptions = highlightOptions(color="#FFCC99",opacity = 0.5, weight = 2, fillOpacity = 0.5,

                                                                                                                                   
                                                                                                                                   
                                                                                                                                                                                                                                                                      bringToFront = TRUE, sendToBack = TRUE))
#la
la_dat <-readOGR(downloaddir, "zipcode_la")
la_transformed<-spTransform(la_dat,CRS=crsmerc)
la_label<-paste(la_transformed$ZIPCODE)
la_map<-leaflet(la_transformed) %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#0066CC",highlightOptions = highlightOptions(color="#FFCC99",opacity = 0.5, weight = 2, fillOpacity = 0.5,
                        bringToFront = TRUE, sendToBack = TRUE),label=la_label,labelOptions = labelOptions(clickable=T,style = list(
                          "color" = "black",
                          "font-family" = "serif",
                          "font-style" = "italic",
                          "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                          "font-size" = "12px",
                          "border-color" = "rgba(0,0,0,0.5)")))    
#sfo
sfo_dat <-readOGR(downloaddir, "zipcode_sfo")
sfo_transformed<-spTransform(sfo_dat,CRS=crsmerc)
sfo_label<-paste(sfo_transformed$zip,"in",sfo_transformed$po_name)
sfo_map<-leaflet(sfo_transformed) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#FF6666",highlightOptions = highlightOptions(color="#FFCC99",opacity = 0.5, weight = 2, fillOpacity = 0.5,
              bringToFront = TRUE, sendToBack = TRUE),label=sfo_label,labelOptions = labelOptions(clickable=T,style = list("color" = "black",
              "font-family" = "serif",
               "font-style" = "italic",
               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                "font-size" = "12px",
                 "border-color" = "rgba(0,0,0,0.5)")))  

#chicago
chi_dat <-readOGR(downloaddir, "zipcode_chi")
chi_transformed<-spTransform(chi_dat,CRS=crsmerc)
chi_label<-paste(chi_transformed$zip)
chi_map<-leaflet(chi_transformed) %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#000066",highlightOptions = highlightOptions(color="#FFCC99",opacity = 0.5, weight = 2, fillOpacity = 0.5,
               bringToFront = TRUE, sendToBack = TRUE),label=chi_label,labelOptions = labelOptions(clickable=T,style = list("color" = "black",
              "font-family" = "serif",
              "font-style" = "italic",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "12px",
              "border-color" = "rgba(0,0,0,0.5)")))                                                                                                                                                                                                              

#Austin
aus_dat <-readOGR(downloaddir, "zipcode_aus")
aus_transformed<-spTransform(aus_dat,CRS=crsmerc)
aus_label<-paste(aus_transformed$zipcode,"in",aus_transformed$name)
aus_map<-leaflet(aus_transformed) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(options = pathOptions(),opacity=0.8,weight=1,color="#336600",highlightOptions = highlightOptions(color="#FFCC99",opacity = 0.5, weight = 2, fillOpacity = 0.5,
              bringToFront = TRUE, sendToBack = TRUE),label=aus_label,
              labelOptions = labelOptions(clickable=T,style = list("color" = "black","font-family" = "serif",
                                                                   "font-style" = "italic",
                                                                   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                   "font-size" = "12px",
                                                                   "border-color" = "rgba(0,0,0,0.5)")))   
                                                                                              