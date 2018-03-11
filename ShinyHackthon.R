library(shiny)
library(leaflet)
library(RColorBrewer)
setwd("~/Desktop/hackathon")
gp.no.patients<- read.csv("gpNoPatientsData.csv", header=T)
gp.no.patients<- na.omit(gp.no.patients)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "MyVar", min(gp.no.patients$totalPatients),
                            max(gp.no.patients$totalPatients),
                            value = range(gp.no.patients$totalPatients), step = 0.1
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[gp.no.patients$totalPatients >= input$range[1] & gp.no.patients$totalPatients <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric("Blues", gp.no.patients$totalPatients)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(gp.no.patients$lat), ~min(gp.no.patients$lat),
                ~max(gp.no.patients$long), ~max(gp.no.patients$lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~1, weight = 10, color = "#777777",
                 fillColor = ~pal(gp.no.patients$totalPatients), fillOpacity = 0.7, popup = ~paste(gp.no.patients$totalPatients)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = gp.no.patients)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~gp.no.patients$totalPatients
      )
    }
  })
}

shinyApp(ui, server)

