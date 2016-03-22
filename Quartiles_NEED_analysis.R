# Set-up ---------------------------
# Load packages
library(threadr)
library(ggplot2)
library(dplyr)
library(tidyr)
library (devtools)
library(readxl)
library(Hmisc)
library(curl)
library(gridExtra)
library(grid)
library(gtable)

#devtools::install_github("skgrange/threadr")# Set global options

options(stringsAsFactors = FALSE)

# Set working directory
setwd("C:/NEED")
# setwd("C:/RICARDO-AEA/NEED")

# Clear all objects
rm(list = ls(all = TRUE))

# Energy NEED consumption data ---------------------------
# Load data

# data_energy <- read.csv("university_of_warwick_energy_data.csv.bz2")
data_energy <- read.csv("need_public_use_file_2014.csv")
#names(data_energy) <- str_to_lower(names(data_energy))
data_energy_gas <- data_energy %>%
  filter(!is.na(IMD_ENG)) %>%
  filter(MAIN_HEAT_FUEL != 2) %>%       ## keep only gas data (1)
  filter(REGION != "W99999999") %>%    ## remove Wales region
  filter(grepl("V", Gcons2012Valid, ignore.case = TRUE)) %>%
  filter(grepl("V", Econs2012Valid, ignore.case = TRUE))

 data_energy_gas$REGION <- ifelse(grepl("E12000001", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "North East", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000002", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "North West", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000003", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "Yorkshire and The Humber", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000004", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "East Midlands", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000005", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "West Midlands", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000006", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "East of England", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000007", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "London", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000008", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "South East", data_energy_gas$REGION)
 data_energy_gas$REGION <- ifelse(grepl("E12000009", data_energy_gas$REGION, ignore.case = TRUE), 
                                  "South West", data_energy_gas$REGION)
  
 data_energy_gas$PROP_TYPE <- ifelse(grepl("101", data_energy_gas$PROP_TYPE, ignore.case = TRUE), 
                                  "Detached house", data_energy_gas$PROP_TYPE)
 data_energy_gas$PROP_TYPE <- ifelse(grepl("102", data_energy_gas$PROP_TYPE, ignore.case = TRUE), 
                                     "Semi-detached house", data_energy_gas$PROP_TYPE)
 data_energy_gas$PROP_TYPE <- ifelse(grepl("103", data_energy_gas$PROP_TYPE, ignore.case = TRUE), 
                                     "End terrace house", data_energy_gas$PROP_TYPE)
 data_energy_gas$PROP_TYPE <- ifelse(grepl("104", data_energy_gas$PROP_TYPE, ignore.case = TRUE), 
                                     "Mid terrace house", data_energy_gas$PROP_TYPE)
 data_energy_gas$PROP_TYPE <- ifelse(grepl("105", data_energy_gas$PROP_TYPE, ignore.case = TRUE), 
                                     "Bungalow", data_energy_gas$PROP_TYPE)
 data_energy_gas$PROP_TYPE <- ifelse(grepl("106", data_energy_gas$PROP_TYPE, ignore.case = TRUE), 
                                     "Flat (inc. maisonette)", data_energy_gas$PROP_TYPE)


data_energy_gas$LOFT_DEPTH[data_energy_gas$LOFT_DEPTH==99] <- NA


data_select_gas <- data_energy_gas %>% 
  select(-IMD_WALES,
         -Gcons2005, -Gcons2005Valid,
         -Gcons2006, -Gcons2006Valid,
         -Gcons2007, -Gcons2007Valid,
         -Gcons2008, -Gcons2008Valid,
         -Gcons2009, -Gcons2009Valid,
         -Gcons2010, -Gcons2010Valid,
         -Gcons2011, -Gcons2011Valid,
         -Econs2005, -Econs2005Valid,
         -Econs2006, -Econs2006Valid,
         -Econs2007, -Econs2007Valid,
         -Econs2008, -Econs2008Valid,
         -Econs2009, -Econs2009Valid,
         -Econs2010, -Econs2010Valid,
         -Econs2011, -Econs2011Valid)

# data_energy_gas <- sample_n(data_energy_gas, 20000)
data_select_gas$inv_IMD_ENG <- 1/(data_select_gas$IMD_ENG)
data_select_gas$energy <- (data_select_gas$Gcons2012)+(data_select_gas$Econs2012)
data_select_gas$energy_floor_area <- (data_select_gas$energy)/(data_select_gas$FLOOR_AREA_BAND)
data_select_gas$gas_floor_area <- (data_select_gas$Gcons2012)/(data_select_gas$FLOOR_AREA_BAND)
data_select_gas$electricity_floor_area <- (data_select_gas$Econs2012)/(data_select_gas$FLOOR_AREA_BAND)
data_select_gas$energy_inv_IMD_ENG <- (data_select_gas$energy)/(1/(data_select_gas$IMD_ENG))
data_select_gas$energy_IMD_ENG <- (data_select_gas$energy)/(data_select_gas$IMD_ENG)
data_select_gas$energy_EE_BAND <- (data_select_gas$energy)/(data_select_gas$EE_BAND)


########## Ranking ###########################################

data_select_gas <- data_select_gas %>%
subset(PROP_TYPE =="Flat (inc. maisonette)")
# subset(PROP_TYPE =="Detached house")
# subset(PROP_TYPE =="Bungalow")
# subset(PROP_TYPE =="Semi-detached house")
# subset(PROP_TYPE =="Mid terrace house")
# subset(PROP_TYPE =="End terrace house")

RANKING_NEED_ENERGY <- data_select_gas %>% 
  mutate(quartile_energy = ntile(energy, 4)) %>% 
  mutate(quartile_area = ntile(energy_floor_area, 4)) %>% 
 #mutate(quartileBounds_energy = cut2(energy, g=8))%>%
  mutate(quartile_IMD = ntile(energy_IMD_ENG, 4)) %>% 
  mutate(quartile_EE = ntile(energy_EE_BAND, 4)) %>% 
  #mutate(quartileBounds_IMD = cut2(energy_IMD_ENG, g=8))%>%
  ungroup()
  

# Arrange by numeric values
RANKING_NEED_ENERGY <- RANKING_NEED_ENERGY %>% 
  arrange(quartile_energy, energy)




##### Logarithmic converisons #######

RANKING_NEED_ENERGY$energy <- log(RANKING_NEED_ENERGY$energy)
RANKING_NEED_ENERGY$energy_floor_area <- log(RANKING_NEED_ENERGY$energy_floor_area)
RANKING_NEED_ENERGY$energy_IMD_ENG <- log(RANKING_NEED_ENERGY$energy_IMD_ENG)
RANKING_NEED_ENERGY$energy_EE_BAND <- log(RANKING_NEED_ENERGY$energy_EE_BAND)


### Subset quartiles ####

select_energy_quartile_1 <- RANKING_NEED_ENERGY %>%
  subset(quartile_energy == 1)
select_energy_quartile_2 <- RANKING_NEED_ENERGY %>%
  subset(quartile_energy == 2)
select_energy_quartile_3 <- RANKING_NEED_ENERGY %>%
  subset(quartile_energy == 3)
select_energy_quartile_4 <- RANKING_NEED_ENERGY %>%
  subset(quartile_energy == 4)


#Now, combine all dataframes from each quartiles into one.  First make a new column in each.
RANKING_NEED_ENERGY$bind <- 'all'
select_energy_quartile_1$bind <- 'quartile_1'
select_energy_quartile_2$bind <- 'quartile_2'
select_energy_quartile_3$bind <- 'quartile_3'
select_energy_quartile_4$bind <- 'quartile_4'


# RANKING_NEED_ENERGY$bind_area <- RANKING_NEED_ENERGY$quartile_area
# select_energy_quartile$bind_area <- 'quartile_1'

QUARTILE_1_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_quartile_1)
QUARTILE_2_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_quartile_2)
QUARTILE_3_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_quartile_3)
QUARTILE_4_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_quartile_4)


### Find quantile positions for each energy signature #######

quantile_energy <- quantile(RANKING_NEED_ENERGY$energy, prob = c(0.25, 0.5, 0.75, 1))
quantile_energy_area <- quantile(RANKING_NEED_ENERGY$energy_floor_area, prob = c(0.25, 0.5, 0.75, 1))
quantile_energy_IMD <- quantile(RANKING_NEED_ENERGY$energy_IMD_ENG, prob = c(0.25, 0.5, 0.75, 1))
quantile_energy_EE <- quantile(RANKING_NEED_ENERGY$energy_EE_BAND, prob = c(0.25, 0.5, 0.75, 1))


index <- c(1:4)
LIST_ALL <-list(QUARTILE_1_ALL,
            QUARTILE_2_ALL,
            QUARTILE_3_ALL,
            QUARTILE_4_ALL)


### Energy

for (i in 1:4) {
QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))

QUARTILE_energy <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"energy",sep="_"),
                         
  ggplot(QUARTILE_ALL,aes(energy, fill = bind)) + 
  geom_density(alpha = 0.5, aes(y = ..count..)) +
  geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=c(1, 2, 3, 4)) +
  scale_x_continuous(breaks=quantile_energy) +
  scale_x_reverse() +
  theme(legend.position = "none") +
  ggtitle(paste("Energy (2012) Quartile", (index[i]))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=14, colour = "black")) + coord_flip() )

}


for (i in 1:4) {
QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
QUARTILE_energy_hist <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"energy_hist",sep="_"),
                           
  ggplot(QUARTILE_ALL,aes(quartile_energy, fill = bind)) + 
  geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
  scale_x_continuous(breaks=c(1,2,3,4))+
  theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
  scale_x_reverse() +
  theme(legend.position = "none") +
  ggtitle(paste("Energy (2012) Quartile", (index[i]))) +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=14, colour = "black")) + coord_flip())
}



#### Energy / Floor Area

for (i in 1:4) {
  QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  QUARTILE_area <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"area",sep="_"),
                           
                           ggplot(QUARTILE_ALL,aes(energy_floor_area, fill = bind)) + 
                             geom_density(alpha = 0.5, aes(y = ..count..)) +
                             geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
                             scale_x_continuous(breaks=c(1, 2, 3, 4)) +
                             scale_x_continuous(breaks=quantile_energy) +
                             scale_x_reverse() +
                             theme(legend.position = "none") +
                             ggtitle(paste("Energy / Floor Area (2012) Quartile", (index[i]))) + 
                             theme(plot.title = element_text(lineheight=.8, face="bold",
                                                             size=14, colour = "black")) + coord_flip())
}
 


for (i in 1:4) {
  QUARTILE_ALL <- assign(paste(paste("  QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  QUARTILE_area_hist <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"area_hist",sep="_"),
                                
                                ggplot(  QUARTILE_ALL,aes(quartile_area, fill = bind)) + 
                                  geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
                                  scale_x_continuous(breaks=c(1,2,3,4))+
                                  theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
                                  scale_x_reverse() +
                                  theme(legend.position = "none") +
                                ggtitle(paste("Energy / Floor Area (2012) Quartile", (index[i]))) + 
                                  theme(plot.title = element_text(lineheight=.8, face="bold",
                                    size=14, colour = "black")) + coord_flip())
}



### Energy / IMD

for (i in 1:4) {
  QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  QUARTILE_IMD <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"IMD",sep="_"),
                         
                         ggplot(QUARTILE_ALL,aes(energy_IMD_ENG, fill = bind)) + 
                           geom_density(alpha = 0.5, aes(y = ..count..)) +
                           geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
                           scale_x_continuous(breaks=c(1, 2, 3, 4)) +
                           scale_x_continuous(breaks=quantile_energy) +
                           scale_x_reverse() +
                           theme(legend.position = "none") +
                           ggtitle(paste("Energy / IMD (2012) Quartile", (index[i]))) + 
                           theme(plot.title = element_text(lineheight=.8, face="bold",
                                                           size=14, colour = "black")) + coord_flip())
} 
  


for (i in 1:4) {
  QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  QUARTILE_IMD_hist <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"IMD_hist",sep="_"),
                              
                              ggplot(QUARTILE_ALL,aes(quartile_IMD, fill = bind)) + 
                                geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
                                scale_x_continuous(breaks=c(1,2,3,4))+
                                theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
                                scale_x_reverse() +  
                                theme(legend.position = "none") +
                                ggtitle(paste("Energy / IMD (2012) Quartile", (index[i]))) + 
                                theme(plot.title = element_text(lineheight=.8, face="bold",
                                                                size=14, colour = "black")) + coord_flip())
}


### Energy / EE

for (i in 1:4) {
  QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  QUARTILE_EE <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"EE",sep="_"),
                         
                         ggplot(QUARTILE_ALL,aes(energy_EE_BAND, fill = bind)) + 
                           geom_density(alpha = 0.5, aes(y = ..count..)) +
                           geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
                           scale_x_continuous(breaks=c(1, 2, 3, 4)) +
                           scale_x_continuous(breaks=quantile_energy) +
                           scale_x_reverse() +  
                           theme(legend.position = "none") +
                           ggtitle(paste("Energy / EE (2012) Quartile", (index[i]))) + 
                           theme(plot.title = element_text(lineheight=.8, face="bold",
                                                           size=14, colour = "black")) + coord_flip())
}



for (i in 1:4) {
  QUARTILE_ALL <- assign(paste(paste("QUARTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  QUARTILE_EE_hist <-  assign(paste(paste("QUARTILE",index[i],sep="_"),"EE_hist",sep="_"),
                             
                             ggplot(QUARTILE_ALL,aes(quartile_EE, fill = bind)) + 
                               geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
                               scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))+
                               theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
                               scale_x_reverse() +  
                               theme(legend.position = "none") +
                               ggtitle(paste("Energy / EE (2012) Quartile", (index[i]))) + 
                               theme(plot.title = element_text(lineheight=.8, face="bold",
                                                               size=14, colour = "black")) + coord_flip())
}


#### Arrange plots ######


jpeg('C:/NEED/plots/octiles_plots/Quartiles_densities.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

grid.arrange(QUARTILE_1_energy, QUARTILE_1_area, QUARTILE_1_IMD, QUARTILE_1_EE,
             QUARTILE_2_energy, QUARTILE_2_area, QUARTILE_2_IMD, QUARTILE_2_EE,
             QUARTILE_3_energy, QUARTILE_3_area, QUARTILE_3_IMD, QUARTILE_3_EE,
             QUARTILE_4_energy, QUARTILE_4_area, QUARTILE_4_IMD, QUARTILE_4_EE, 
             ncol=4, nrow =4,
             top=textGrob("Flat (inc. maisonette)",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Bungalow",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Semi-detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Mid terrace house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("End terrace house",gp=gpar(fontsize=20,font=3)))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/octiles_plots/Quartiles_hist.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

grid.arrange(QUARTILE_1_energy_hist, QUARTILE_1_area_hist, QUARTILE_1_IMD_hist, QUARTILE_1_EE_hist,
             QUARTILE_2_energy_hist, QUARTILE_2_area_hist, QUARTILE_2_IMD_hist, QUARTILE_2_EE_hist,
             QUARTILE_3_energy_hist, QUARTILE_3_area_hist, QUARTILE_3_IMD_hist, QUARTILE_3_EE_hist,
             QUARTILE_4_energy_hist, QUARTILE_4_area_hist, QUARTILE_4_IMD_hist, QUARTILE_4_EE_hist,
             ncol=4, nrow =4,               
             top=textGrob("Flat (inc. maisonette)",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Bungalow",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Semi-detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Mid terrace house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("End terrace house",gp=gpar(fontsize=20,font=3)))


par(oldpar)
dev.off()

###################################################################################
###################################################################################
###################################################################################
###################################################################################

