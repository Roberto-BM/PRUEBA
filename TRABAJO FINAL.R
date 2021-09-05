library(shiny)
library(sf)
library(leaflet)
library(dplyr)

Edu4 <- read.csv("D:/UNMSM/Progra/Nivel_edu4.csv")
Edu4 <- data.frame(Edu4)
Edu4 <- as.tibble(Edu4)
Edu4[1]

ui <- fluidPage(
  
  titlePanel("Nivel Educativo por distritos"),
  sidebarLayout(
    
    sidebarPanel(
      textInput(inputId = "caption",
                label = "Title:",
                value = "Resumen de datos"),
      
      
      selectInput(inputId = "distritos",
                  label = "Distritos:",
                  choices = Edu4[1]),
      
      
      numericInput(inputId = "obs",
                   label = "Ver distritos:",
                   value = 5 )
    ),
    
    mainPanel(
      
      h3(textOutput("Resumen")),
      tableOutput("view"),
      plotOutput("phonePlot")
      
    )
  ),
  leafletOutput("map")
)

server <- function(input, output) {
  
  
  output$view <- renderTable({
    head(Edu4, n = input$obs)
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  output$phonePlot <- renderPlot({
    barplot(Edu3[,input$distritos], 
            main=input$distritos,
            names=Edu3$distritos,
            col="red",
            xlam=c(2,5))
    
    legend("topright", legend = c("S_U_I: Superior Universitaria Incompleta",
                                  "S_U_C: Superior Universitaria Completa",
                                  "S_N_U_I: Superior NO Universitaria Incompleta",
                                  "S_N_U_C: Superior NO Universitaria Completa") , 
           bty = "n", pch=4, pt.cex = 0.8, cex = 1.2, horiz = FALSE, inset = c(0.01,-0.02))
    
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=dist$X, lat=dist$Y, popup = dist$NOMCCPP)%>%
      addCircles(lng =dist$X, lat =dist$Y, radius = 300, color = "orange")
  })
  
}

shinyApp(ui, server)


