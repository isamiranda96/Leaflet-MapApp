library(osmdata)
library(shiny)
library(leaflet)
library(rgdal)
library(geojsonio)
library(sf)
library(tibble)
library(spdplyr)
library(DescTools)
library(htmltools)
library(shinyWidgets)
library(purrr)
library(shinythemes)
library(shinyjs)
library(rsconnect)
library(wesanderson)
library(RColorBrewer)


b <- readOGR("kml data/Bistros Saúl.kml", "Bistros Saúl")%>%
    mutate(Description = "Bistros",
           Marca = "Saul")

c <- readOGR("kml data/Cafes.kml", "Cafes")%>%
    mutate(Description = "Cafes",
           Marca = "Saul")


sm <- readOGR("kml data/San Martín.kml", "San Martín")%>%
    mutate(Description = ifelse(Name %like% "%Subway%" |
                                Name %like% "%El Maestro%" |
                                Name %like% "%Pacific%" |
                                Name %like% "%Aguilar%" |
                                Name %like% "%Sebastian%"
                                , "Panaderia","Restaurante"),
           Marca = "San Martin")



jet.colors <-
    colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                       "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


# gt <- readOGR("guatemala-latest.osm.pbf", "points", encoding = "UTF-8")
# 
# 
# gt
# 
# read("m_guatemala.xml")
# 
# ogrListLayers("guatemala-latest.osm.pbf")
# ogrInfo("guatemala-latest.osm.pbf")


#----------propuestas---------

coor <- rbind(cbind(-90.538545, 14.593980,0), #CDP Saul
              cbind(-90.545863, 14.562334,0), #Los Cedros
              cbind(-90.506345, 14.651118,0)  #el zapote
              ) 
    

datos <- tibble(Name = c("CDP Saul",
                         "Los Cedros",
                         "Mercado La Parroquia"), 
                Description = "Cocina Fantasma",
                Marca = "Saul")

myCRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

nuevas <- SpatialPointsDataFrame(coords = coor, data = datos, proj4string = myCRS)

#---- ICONOS ----------

iconos <- iconList(
    Saul = makeIcon(iconUrl = "imagenes/saul.png", iconWidth = 42, iconHeight = 65),
    `San Martin` = makeIcon(iconUrl = "imagenes/sm.png", iconWidth = 42, iconHeight = 65)
)

#----------APP------------------

opciones <- as.vector(b$Name)%>%
                set_names(as.vector(b$Name))

opciones_c <- as.vector(c$Name)%>%
    set_names(as.vector(c$Name))

opciones_n <- as.vector(nuevas$Name)%>%
    set_names(as.vector(nuevas$Name))

opciones_sm_res <- as.vector(sm$Name[sm$Description == "Restaurante"])%>%
    set_names(as.vector(sm$Name[sm$Description == "Restaurante"]))

opciones_sm_pan <- as.vector(sm$Name[sm$Description != "Restaurante"])%>%
    set_names(as.vector(sm$Name[sm$Description != "Restaurante"]))


ui <- bootstrapPage(
                
    navbarPage("MapApp", collapsible = TRUE, id="nav",
    
    tabPanel("Medidor de Cobertura",
             
             div(class="outer",
                    
                    shiny::tags$head(includeCSS("styles.css")),
                    leafletOutput("map", width = "100%", height = "105%"),
        
                    absolutePanel(top = 20, right=20, width = "20%",
                                  id = "controls", class = "panel panel-default",
                                
                                column(12,
                                       
                                       useShinyjs(),
                                       
                                       br(),
                                       
                                       fluidRow(class = "panel", 
                                           
                                              column(5,
                                                     
                                              materialSwitch(
                                                  inputId = "s",
                                                  label = "Saúl", 
                                                  value = TRUE,
                                                  status = "danger"
                                              ),
                                              
                                              ),
                                              
                                              column(7,
                                                     
                                              materialSwitch(
                                                  inputId = "sm",
                                                  label = "San Martín", 
                                                  value = FALSE,
                                                  status = "primary"
                                                  
                                                  ),
                                              ),
                                            ),
                                            
                                       fluidRow(
                
                                              pickerInput("unidades", "Seleccionar Bistros: ",
                                                          choices = opciones, 
                                                          options = list(
                                                              `actions-box` = TRUE, 
                                                                size = 10,
                                                              `selected-text-format` = "count > 1",
                                                              `count-selected-text` = "{0}/{1} Bistros"
                                                          ),
                                                          multiple = TRUE),
                                              br(),
                                              pickerInput("unicaf", "Seleccionar Cafés: ",
                                                          choices = opciones_c, 
                                                          options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 1",
                                                              `count-selected-text` = "{0}/{1} Cafés"
                                                          ),
                                                          multiple = TRUE),
                                              br(),
                                              pickerInput("cf", "Seleccionar Cocinas Fantasma: ",
                                                          choices = opciones_n, 
                                                          options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 1",
                                                              `count-selected-text` = "{0}/{1} Cocinas"
                                                          ),
                                                          multiple = TRUE),
                                              br(),
                                              
                                              pickerInput("sanm", "Seleccionar Restaurantes: ",
                                                          choices = opciones_sm_res, 
                                                          options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 1",
                                                              `count-selected-text` = "{0}/{1} Restaurantes"
                                                          ),
                                                          multiple = TRUE),
                                              
                                              br(),
                                              
                                              pickerInput("sanm_pan", "Seleccionar Panaderias: ",
                                                          choices = opciones_sm_pan, 
                                                          options = list(
                                                              `actions-box` = TRUE, 
                                                              size = 10,
                                                              `selected-text-format` = "count > 1",
                                                              `count-selected-text` = "{0}/{1} Panaderías"
                                                          ),
                                                          multiple = TRUE),
                                              
                                              br(),
                                              
                                              sliderInput("range", "Radio de Cobertura (km): ", min(0), max(8),
                                                          value = 4, step = 0.5)
                                              )
                                        )
                                    )
                )
            )
    
    # tabPanel("POI Data",
    #          
    #          div(class="outer",
    #              
    #              shiny::tags$head(includeCSS("styles.css")),
    #              leafletOutput("mapoi", width = "100%", height = "105%")
                 
                 # absolutePanel(top = 20, right=20, width = "20%",
                 #               id = "controls", class = "panel panel-default",
                 #               
                 #               column(12,
                 #                      
                 #                      useShinyjs(),
                 #                      
                 #                      br(),
                 #                      
                 #                      fluidRow(class = "panel", 
                 #                               
                 #                               column(5,
                 #                                      
                 #                                      materialSwitch(
                 #                                          inputId = "s",
                 #                                          label = "Saúl", 
                 #                                          value = TRUE,
                 #                                          status = "danger"
                 #                                      ),
                 #                                      
                 #                               ),
                 #                               
                 #                               column(7,
                 #                                      
                 #                                      materialSwitch(
                 #                                          inputId = "sm",
                 #                                          label = "San Martín", 
                 #                                          value = FALSE,
                 #                                          status = "primary"
                 #                                          
                 #                                      ),
                 #                               ),
                 #                      ),
                 #                      
                 #                      fluidRow(
                 #                          
                 #                          pickerInput("unidades", "Seleccionar Bistros: ",
                 #                                      choices = opciones, 
                 #                                      options = list(
                 #                                          `actions-box` = TRUE, 
                 #                                          size = 10,
                 #                                          `selected-text-format` = "count > 1",
                 #                                          `count-selected-text` = "{0}/{1} Bistros"
                 #                                      ),
                 #                                      multiple = TRUE),
                 #                          br(),
                 #                          pickerInput("unicaf", "Seleccionar Cafés: ",
                 #                                      choices = opciones_c, 
                 #                                      options = list(
                 #                                          `actions-box` = TRUE, 
                 #                                          size = 10,
                 #                                          `selected-text-format` = "count > 1",
                 #                                          `count-selected-text` = "{0}/{1} Cafés"
                 #                                      ),
                 #                                      multiple = TRUE),
                 #                          br(),
                 #                          pickerInput("cf", "Seleccionar Cocinas Fantasma: ",
                 #                                      choices = opciones_n, 
                 #                                      options = list(
                 #                                          `actions-box` = TRUE, 
                 #                                          size = 10,
                 #                                          `selected-text-format` = "count > 1",
                 #                                          `count-selected-text` = "{0}/{1} Cocinas"
                 #                                      ),
                 #                                      multiple = TRUE),
                 #                          br(),
                 #                          
                 #                          pickerInput("sanm", "Seleccionar Restaurantes: ",
                 #                                      choices = opciones_sm_res, 
                 #                                      options = list(
                 #                                          `actions-box` = TRUE, 
                 #                                          size = 10,
                 #                                          `selected-text-format` = "count > 1",
                 #                                          `count-selected-text` = "{0}/{1} Restaurantes"
                 #                                      ),
                 #                                      multiple = TRUE),
                 #                          
                 #                          br(),
                 #                          
                 #                          pickerInput("sanm_pan", "Seleccionar Panaderias: ",
                 #                                      choices = opciones_sm_pan, 
                 #                                      options = list(
                 #                                          `actions-box` = TRUE, 
                 #                                          size = 10,
                 #                                          `selected-text-format` = "count > 1",
                 #                                          `count-selected-text` = "{0}/{1} Panaderías"
                 #                                      ),
                 #                                      multiple = TRUE),
                 #                          
                 #                          br(),
                 #                          
                 #                          sliderInput("range", "Radio de Cobertura (km): ", min(0), max(8),
                 #                                      value = 4, step = 0.5)
                 #                      )
                 #               )
                 # )
    #          )
    # )
    
    
    
        )
    )


server <- function(input, output, session) {
    
    observe({
        
        if(input$s == FALSE){
            shinyjs::hide("unidades")
            shinyjs::hide("unicaf")
            shinyjs::hide("cf")
            updatePickerInput(session, "unidades", selected = "")
            updatePickerInput(session, "unicaf", selected = "")
            updatePickerInput(session, "cf", selected = "")
        }
        
        if(input$s == TRUE){
            shinyjs::show("unidades")
            shinyjs::show("unicaf")
            shinyjs::show("cf")
            
        }
        
    })
    
    observe({
        
        if(input$sm == FALSE){
            shinyjs::hide("sanm")
            shinyjs::hide("sanm_pan")
            updatePickerInput(session, "sanm", selected = "")
            updatePickerInput(session, "sanm_pan", selected = "")
        }
        if(input$sm == TRUE){
            shinyjs::show("sanm")
            shinyjs::show("sanm_pan")
        }
        
    })
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        
        lista <- input$unidades
        lista2 <- input$unicaf
        lista3 <- input$cf
        lista4 <- input$sanm
        lista5 <- input$sanm_pan
        
        temp <- rbind(b, c, nuevas, sm)%>%
            filter(Name %in% lista | Name %in% lista2 | Name %in% lista3 | Name %in% lista4 
                   | Name %in% lista5)
        
        temp
        
    })

    output$map <- renderLeaflet({
        
            leaflet()%>%
                addTiles()%>%
                addProviderTiles(providers$CartoDB.PositronNoLabels,
                                 options = providerTileOptions(opacity = 0.15))%>%
                setView(lat = 14.595136, lng =  -90.511672, zoom = 12)

    })
    
    # Actualizar mapa acorde a inputs
    observe({

        if((length(input$unidades) > 0L | length(input$unicaf) > 0L |
           length(input$cf) > 0L | length(input$sanm) > 0L | length(input$sanm_pan) > 0L)){

            leafletProxy("map",data = filteredData())%>%
                clearMarkers()%>%
                clearShapes()%>%
                addMarkers(label = ~as.character(Name),
                           labelOptions = labelOptions(clickable = TRUE),
                           icon = ~iconos[Marca])%>%
                addCircles(radius=input$range*1000, stroke=0,
                           fillColor = ifelse(filteredData()$Description == "Bistros","Red",
                                              ifelse(filteredData()$Description == "Cafes", "Black", 
                                                     ifelse(filteredData()$Marca %like% "%Mart%","Navy","Yellow")))
                           ,color = ifelse(filteredData()$Description == "Bistros","Red",
                                        ifelse(filteredData()$Description == "Cafes", "Black",
                                               ifelse(filteredData()$Marca %like% "%Mart%","Navy","Yellow")))
                           )
        }

        else{
            
            leafletProxy("map")%>%
                clearMarkers()%>%
                clearShapes()
        }
        
    })
    
    
    output$mapoi <- renderLeaflet({
        
        
        leaflet()%>%
            addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                     attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>%
            setView(lat = 14.595136, lng =  -90.511672, zoom = 12)
        
    })
    
    
    # observe({
    #     
    #     
    #     if((length(input$unidades) > 0L | length(input$unicaf) > 0L |
    #         length(input$cf) > 0L | length(input$sanm) > 0L | length(input$sanm_pan) > 0L)){
    #         
    #         
    #         sf_df <- st_as_sf(filteredData(), coords = c("lon", "lat"))
    #         
    #         sf_circles <- st_buffer(sf_df, dist = input$range/100)
    #         
    #         #sf_combined <- st_union(sf_circles)
    #         
    #         data <- opq(bbox = st_bbox(sf_circles)) %>%
    #             #add_osm_feature(key = 'shop')%>%
    #             add_osm_feature(key = 'amenity')%>%
    #             osmdata_sf()
    #         
    #         
    #         dfinal <- st_intersection(st_as_sf(data$osm_points), st_as_sf(sf_circles))%>%
    #             select(Name, name, amenity, geometry)%>%
    #             filter(!is.na(amenity))%>%
    #             mutate(amenity = ifelse(amenity == "restaurant" |
    #                                     amenity == "bank" |
    #                                     amenity == "fast_food" |
    #                                     amenity == "parking" |
    #                                     amenity == "pharmacy" |
    #                                     amenity == "cafe", amenity, "other"))
    #         
    #         pal <- colorFactor(rainbow(length(unique(dfinal$amenity))), unique(dfinal$amenity))
    #         
    # 
    #         leafletProxy("mapoi", data = dfinal)%>%
    #             clearMarkers()%>%
    #             clearShapes()%>%
    #             addCircleMarkers(label = ~as.character(paste("Nombre: ", name, "</br>",
    #                                                          "Categoria: ", amenity)),
    #                        labelOptions = labelOptions(clickable = TRUE),
    #                        color=~pal(amenity), stroke = F, fillOpacity = 0.5, weight = 3, radius=4)%>%
    #             addLegend("bottomright", pal = pal, values = ~amenity,
    #                       title = "CATEGORÍA",
    #                       opacity = 1)
    #             
    #         
    #     }
    #     
    #     else{
    #         
    #         leafletProxy("mapoi")%>%
    #             clearMarkers()%>%
    #             clearShapes()
    #     }
    #     
    # })
    
    
}

shinyApp(ui, server)
