
library(shiny)
library(dygraphs)

require(rCharts)
shinyUI(pageWithSidebar(
  headerPanel("rCharts: Interactive Charts from R using polychart.js"),
  
  sidebarPanel(
      sliderInput("range_energy", 
                  label = ("Total energy:"),
                  min = 100, max = 75000, 
                  value = c(3000, 30000))
    ),
    

  mainPanel(
    dygraphOutput("dygraph")
    
  )
))




