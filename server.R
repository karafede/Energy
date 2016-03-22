
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rCharts)
library(ggplot2)
library(reshape2)
library(devtools)
library(xts)
library(dygraphs)
library(lubridate)
library(stringr)
library(DT)
library(leaflet)



RANKING_NEED_ENERGY <- read.csv("Ranked_NEED_cleaned_2012.csv")
RANKING_NEED_ENERGY <- RANKING_NEED_ENERGY %>% 
  select(energy,
         energy_floor_area,
         energy_IMD_ENG,
         energy_EE_BAND,
         octile_energy,
         octile_area)



##### Logarithmic converisons #######

# RANKING_NEED_ENERGY$energy <- log(RANKING_NEED_ENERGY$energy)
# RANKING_NEED_ENERGY$energy_floor_area <- log(RANKING_NEED_ENERGY$energy_floor_area)
# RANKING_NEED_ENERGY$energy_IMD_ENG <- log(RANKING_NEED_ENERGY$energy_IMD_ENG)
# RANKING_NEED_ENERGY$energy_EE_BAND <- log(RANKING_NEED_ENERGY$energy_EE_BAND)



### Find quantile positions for each energy signature #######

quantile_energy <- quantile(RANKING_NEED_ENERGY$energy, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                 0.75, 0.875, 1))
quantile_energy_area <- quantile(RANKING_NEED_ENERGY$energy_floor_area, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                                 0.75, 0.875, 1))
quantile_energy_IMD <- quantile(RANKING_NEED_ENERGY$energy_IMD_ENG, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                             0.75, 0.875, 1))
quantile_energy_EE <- quantile(RANKING_NEED_ENERGY$energy_EE_BAND, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                                            0.75, 0.875, 1))

require(rCharts)
shinyServer(function(input, output) {
    output$dygraph <- renderDygraph({

     k = row(RANKING_NEED_ENERGY[1])
     N = max(k)
    
     sub <- reactive({subset(RANKING_NEED_ENERGY,
                     energy == input$range_energy) })

    
#     # Convert to counts
     
#    dense_ENE$y = N/sum(dense_ENE$y) * dense_ENE$y
#     dense_ENE = data.frame(dense_ENE$x, dense_ENE$y)
#     
   
    m <- ggplot(sub(), aes(x = log(energy)))
   
#     p1 <- dygraph(density ~ x,
#             data= ggplot_build(m + geom_density())$data[[1]][c("x","density")]) %>%
#       dyRangeSelector()
#     
    
    p1 <- dygraph(density ~ x,
        data= ggplot_build(m + geom_density())$data[[1]][c("x","density")]) %>%
                  dyRangeSelector()

       print(p1)
  })
})

