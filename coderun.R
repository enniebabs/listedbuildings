# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(readr)
library(rsconnect)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)

  
  
data<-read.csv("data/listedbuildings_north.csv")
data <- data %>% filter(!is.na(ListEntry))
View(data)
data <-  st_as_sf(data , coords = c("Easting", "Northing")) %>% #converts to simple feature
  st_set_crs(.,27700) %>% 
  st_transform( ., 4326) %>%# - EPSG code as an integer
  mutate(longitude= st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2])
  
  ui <- bootstrapPage(
    
    tags$head(
      # Include our custom CSS
      includeCSS("./assets/styles.css")
    ),  
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(top = 70, right = 10, id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, height = "auto",                
                  # Application title
                  titlePanel("Listed Buildings in the North of England"),
                  selectInput("region", "Region", choices = unique(data$Region), selected = ""),
                   selectInput("grade", "Grade", choices = unique(data$Grade), selected = "II"),
                  plotOutput("bargraph", height = 200)
                  
                  ),
    
    tags$div(id="copyright",
             p("Data source : Historic England 2021. Contains Ordnance Survey data Crown copyright and 
database right 2021")
    )
  )

server <- function(input, output) {



  
  
# UI Selected input components - reactive
 region <- reactive({
   input$region
 })

grade <- reactive({
  input$grade
})
output$map<-renderLeaflet({

  data <- filter(data, Grade == input$grade, Region == input$region)
  plotMap <- leaflet() %>%
    
    addProviderTiles(providers$CartoDB.Positron, 
                     options = providerTileOptions(minZoom= 1, maxZoom = 25),
                     group = "Open Street Map") %>% 
    addProviderTiles(providers$Esri.WorldImagery, 
                     options = providerTileOptions(minZoom= 1, maxZoom = 25),
                     group = "Satellite") %>% 
    #setView(-1.0828,51.9927, zoom = 8) %>% 
    addMarkers(
      data = data,
      ~longitude, 
      ~latitude, 
      popup = ~as.character(Location),
      clusterOptions = markerClusterOptions()) %>% 
    addLayersControl(baseGroups = c("Open Street Map", "Satellite")
                     )
})
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
 dataInBounds <- reactive({
   data <- filter(data, Grade == input$grade, Region == input$region)
   
    if (is.null(input$map_bounds))
      return(data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    #View(lngRng)
    subset(data,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
 output$bargraph <- renderPlot({
   # If no zipcodes are in view, don't plot
   if (nrow(dataInBounds()) == 0)
     return(NULL)
   xx <-dataInBounds()%>% 
     group_by(Grade)%>% 
     summarise(countrecords = n())
   
  # hist(xx$count,
    #  breaks = xx$Grade,
     #   main = "SuperZIP score (visible zips)",
     #   xlab = "Percentile",
       # xlim = range(allzips$centile),
     #   col = '#00DD00',
      #  border = 'white')
   #View(xx)
   ggplot(data=xx, aes(x=Grade, y=countrecords)) +
     geom_bar(stat="identity", width=0.5)
 })
 
  
 # data <- filter(.data = data, Grade == input$grade)

}
shinyApp(ui = ui, server = server)