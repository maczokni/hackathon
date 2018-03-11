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
gp.no.patients$totalPatientsSmall <- gp.no.patients$totalPatients*0.5
gp.no.patients<- na.omit(gp.no.patients)

gp.no.patients <- gp.no.patients[gp.no.patients$Allergies < 2,]
gp.no.patients <- gp.no.patients[gp.no.patients$Asthma < 2,]
gp.no.patients <- gp.no.patients[gp.no.patients$Diabetes < 2,]





ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, width = 300,
                draggable = TRUE, 
                style = "opacity: 0.9; padding: 8px; background: #FFFFEE;",
                h3("GP Prescription Map"),
                p("This map shows prescriptions issued by each GP for medications for different medical issues. The shading represents the number of prescriptions per 100 patients registered."),
                sliderInput("range", "Select range for number of patients", min(gp.no.patients$totalPatients),
                            max(gp.no.patients$totalPatients),
                            value = range(gp.no.patients$totalPatients), step = 1
                ),
                #selector in progress
                selectInput("select", 
                            label = "Select medical issue", 
                            choices = c("Allergies" ,
                                        "Asthma" ,
                                        "Diabetes" ), 
                            selected = "Diabetes"),
                checkboxInput("legend", "Show legend", TRUE),
                HTML('This map was developed as part of the <a href="https://www.ukdataservice.ac.uk/news-and-events/eventsitem/?id=4760">UKDS Manchester Data Dive</a> using <a href="https://data.gov.uk/dataset/prescribing-by-gp-practice-presentation-level">GP practice prescribing data data from NHS digital</a>. Contains public sector information licensed under the Open Government Licence v3.0.</a>')
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    gp.no.patients[gp.no.patients$totalPatients >= input$range[1] & gp.no.patients$totalPatients <= input$range[2],]
  })

  colorpal <- reactive({
    fd <- filteredData()
    colorBin("Blues", 
             eval(call("$",
                       as.symbol("fd"),input$select)), 5, pretty=TRUE)
  })
  
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>%
      setView(-0.127758, 51.507351, zoom = 5)
  })
  
  
  observe({
    pal <- colorpal()
    fd <- filteredData()
    leafletProxy("map", data = fd) %>%
      clearShapes()  %>%
      addCircleMarkers(color = eval(call("$",
                                         as.symbol("fd"),input$select)),
                       fillColor = ~pal(eval(call("$",
                                                  as.symbol("fd"),input$select))), 
                       fillOpacity = 0.7, 
                       popup = ~paste0("<b>GP:</b> ",
                                       fd$PNAME, 
                                       "<br><b>Number of patients registered:</b> ", 
                                       fd$totalPatients,
                                       "<br><b>Prescriptions per 100 patients:</b> ", 
                                       round(eval(call("$",
                                                       as.symbol("fd"),input$select)),2)))
  })
 
}

shinyApp(ui, server)
