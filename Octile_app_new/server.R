
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


RANKING_NEED_ENERGY <- read.csv("Ranked_NEED_cleaned_2012.csv")
RANKING_NEED_ENERGY <- RANKING_NEED_ENERGY %>% 
  select(energy,
         energy_floor_area,
         energy_IMD_ENG,
         energy_EE_BAND,
         octile_energy,
         octile_area)



##### Logarithmic converisons #######

#  RANKING_NEED_ENERGY$energy <- log(RANKING_NEED_ENERGY$energy)
#  RANKING_NEED_ENERGY$energy_floor_area <- log(RANKING_NEED_ENERGY$energy_floor_area)
#  RANKING_NEED_ENERGY$energy_IMD_ENG <- log(RANKING_NEED_ENERGY$energy_IMD_ENG)
#  RANKING_NEED_ENERGY$energy_EE_BAND <- log(RANKING_NEED_ENERGY$energy_EE_BAND)



### Find quantile positions for each energy signature #######

quantile_energy <- quantile(RANKING_NEED_ENERGY$energy, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                 0.75, 0.875, 1))
quantile_energy_area <- quantile(RANKING_NEED_ENERGY$energy_floor_area, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                                 0.75, 0.875, 1))
quantile_energy_IMD <- quantile(RANKING_NEED_ENERGY$energy_IMD_ENG, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                             0.75, 0.875, 1))
quantile_energy_EE <- quantile(RANKING_NEED_ENERGY$energy_EE_BAND, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                            0.75, 0.875, 1))

# Define server logic required to plot variables
shinyServer(function(input, output) {
  
  
  #Generate a plot of the requested variables
  
  sub_ENE <- reactive({subset(RANKING_NEED_ENERGY,
                    energy == input$range_energy|
                    energy_floor_area == input$range_area | 
                    energy_IMD_ENG == input$range_IMD | 
                    energy_EE_BAND == input$range_EE)
  })


  sub_AREA <- reactive({subset(RANKING_NEED_ENERGY,
                               energy == input$range_energy|
                                 energy_floor_area == input$range_area | 
                                 energy_IMD_ENG == input$range_IMD | 
                                 energy_EE_BAND == input$range_EE)
  })
  
  
  sub_IMD <- reactive({subset(RANKING_NEED_ENERGY,
                              energy == input$range_energy|
                                energy_floor_area == input$range_area | 
                                energy_IMD_ENG == input$range_IMD | 
                                energy_EE_BAND == input$range_EE)
  })
  
  sub_EE <- reactive({subset(RANKING_NEED_ENERGY,
                             energy == input$range_energy|
                               energy_floor_area == input$range_area | 
                               energy_IMD_ENG == input$range_IMD | 
                               energy_EE_BAND == input$range_EE)
  })
                      
                    
  
  output$Fig.1 <- renderPlot({
    ENERGY <-  ggplot(sub_ENE(), aes(log(energy))) +
      geom_density(alpha = 0.5, fill = "red") +
       scale_x_log10() +
      geom_vline(xintercept= log(quantile_energy),
                 color="red", linetype="dashed", size=0.5) 
    print(ENERGY)
  })
  

    
  output$Fig.2 <- renderPlot({
    ENERGY_AREA <-  ggplot(sub_AREA(), aes(log(energy_floor_area))) +
      geom_density(alpha = 0.5, fill= "green") +
      scale_x_log10() +
      geom_vline(xintercept= log(quantile_energy_area),
                 color="red", linetype="dashed", size=0.5) 
    print(ENERGY_AREA)
  })
    
    
    output$Fig.3 <- renderPlot({
      ENERGY_IMD <-  ggplot(sub_IMD(), aes(log(energy_IMD_ENG))) +
        geom_density(alpha = 0.5, fill= "green") +
        scale_x_log10() +
        geom_vline(xintercept= log(quantile_energy_IMD),
                   color="red", linetype="dashed", size=0.5) 
      print(ENERGY_IMD)
})
      
      
      
      output$Fig.4 <- renderPlot({
        ENERGY_EE <-  ggplot(sub_EE(), aes(log(energy_EE_BAND))) +
          geom_density(alpha = 0.5, fill = "yellow") +
          scale_x_log10() +
          geom_vline(xintercept= log(quantile_energy_EE),
                     color="red", linetype="dashed", size=0.5) 
        print(ENERGY_EE)
      })
    })



