library(leaflet)
library(rgdal)
library(geojsonio)
library(sf)
library(tibble)
library(spdplyr)
library(DescTools)
library(htmltools)

ogrListLayers("Bistros Saúl.kml") 

ogrInfo("Bistros Saúl.kml", "Bistros Saúl")

ubi <- readOGR("Food Car.kml","Food Car")


fc <- readOGR("MapaUnidades/Food Car.kml", "Food Car")%>%
  mutate(c)

sb <- readOGR("MapaUnidades/San Martín.kml", "San Martín")

b <- readOGR("MapaUnidades/Bistros Saúl.kml", "Bistros Saúl", encoding = "UTF-8")%>%
  filter(!(Name %like% "%Casa%"),
         !(Name %like% "%15%"),
         !(Name %like% "%Madero%"))

c <- readOGR("MapaUnidades/Cafes.kml", "Cafes")

df <- rbind(b,c)


b


leaflet(b)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addMarkers(label = ~htmlEscape(Name), labelOptions = labelOptions(noHide = T))%>% 
  addCircles(radius=5000, stroke=1, fillColor = "Red", color="Red")



