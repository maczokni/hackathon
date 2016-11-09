library(shiny)
library(leaflet)
library(RColorBrewer)

gp.no.patients<- read.csv("gpNoPatientsData.csv", header=T)
prescriptions <- read.csv("Data_Practice.csv")
names(prescriptions)[names(prescriptions)=="Practice"] <- "PRACTICE_CODE"
gp.no.patients <- merge(prescriptions, gp.no.patients, by="PRACTICE_CODE")
gp.no.patients$Asthma <- gp.no.patients$Pres_Count_Asthma/gp.no.patients$totalPatients*100
gp.no.patients$Allergies <- gp.no.patients$Pres_Count_Allergies/gp.no.patients$totalPatients*100
gp.no.patients$Diabetes <- gp.no.patients$Pres_Count_Diabetes/gp.no.patients$totalPatients*100
gp.no.patients<- na.omit(gp.no.patients)

gp.no.patients <- gp.no.patients[gp.no.patients$Allergies < 2,]

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Select range for number of patients", min(gp.no.patients$totalPatients),
                            max(gp.no.patients$totalPatients),
                            value = range(gp.no.patients$totalPatients), step = 0.1
                ),
                #selector in progress
                selectInput("select", 
                            label = "Select medical issue", 
                            choices = c("Allergies" = gp.no.patients$Allergies,
                                        "Asthma" = gp.no.patients$Asthma,
                                        "Diabetes" = gp.no.patients$Diabetes), 
                            selected = "Allergies"),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    gp.no.patients[gp.no.patients$totalPatients >= input$range[1] & gp.no.patients$totalPatients <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
  #  colorNumeric("Blues", eval(as.symbol(paste0("gp.no.patients", "$", input$select))))
   colorNumeric("Blues", gp.no.patients$Allergies)
#colorNumeric("Blues", input$select)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(gp.no.patients) %>% addTiles() %>%
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
      addCircles(radius = ~totalPatients,  color = "#777777",
                 fillColor = ~pal(Allergies), fillOpacity = 0.7, popup = ~paste0("<b>GP:</b>", 
                                                                                 gp.no.patients$PNAME, 
                                                                                 "    ", "<br><b>Prescription per 100 patients: </b>", 
                                                                                 Allergies)
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
                          pal = pal, values = ~Allergies
      )
    }
  })
}

shinyApp(ui, server)
