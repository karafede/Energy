library(shiny)

# Define UI for iris application
shinyUI(fluidPage(
  titlePanel("Using Shiny with the NEED dataset"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
      
      helpText("Create density plots with NEED dataset"),
      
#       selectInput("variable",
#                   label = "Select variable:",
#                   list("Energy Tot" = "energy",
#                        "Energy/Floor Area"  = "energy_floor_area",
#                        "Energy/IMD" = "energy_IMD_ENG",
#                        "Energy/EE BAND"  = "energy_EE_BAND"),
#                   selected = "energy"),
      
      sliderInput("range_energy", 
                  label = "Total Energy:",
                  min = 100, max = 75000, 
                #  value = 100, step = 100),
                  value = c(100, 100), step =100),      

      sliderInput("range_area", 
                  label = "Energy/Floor Area:",
                  min = 50, max = 65000, 
                  value = c(50, 50), step =100), 
      
      sliderInput("range_IMD", 
                  label = "Energy/IMD:",
                  min = 150, max = 35000, 
                  value = c(150, 150), step =100), 
      
      sliderInput("range_EE", 
                  label = "Energy/EE Band:",
                  min = 100, max = 55000, 
                  value = c(100, 100), step =10)),
    
    mainPanel(
      #       plotOutput("plot_1"),
      #   
      #       mainPanel(
      #         plotOutput("plot_2")
      
      #     tabsetPanel(position = "below",     ## "left" or "right"
      tabPanel("Energy",
               plotOutput("Fig.1"),
               p(paste("Total Energy",
                       " ",
                       " "))),
      
      tabPanel("Energy/Floor Area",
               plotOutput("Fig.2"),
               p(paste("Energy / Floor Area",
                       " ",
                       " "))),
      
      tabPanel("Energy/IMD",
               plotOutput("Fig.3"),
               p(paste("Energy / IMD",
                       " ",
                       " "))),
      
      
      tabPanel("Energy/EE Band",
               plotOutput("Fig.4"),
               p(paste("Energy / EE Band",
                       " ",
                       " ")))
      
      
    )
  )
)
)
# )