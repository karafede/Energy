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

write.csv(data_select_gas, file = "C:/NEED/NEED_cleaned_2012.csv", row.names=FALSE)

########## Ranking ###########################################

# data_select_gas <- data_select_gas %>%
# subset(PROP_TYPE =="Flat (inc. maisonette)")
 #subset(PROP_TYPE =="Detached house")
# subset(PROP_TYPE =="Bungalow")
# subset(PROP_TYPE =="Semi-detached house")
# subset(PROP_TYPE =="Mid terrace house")
# subset(PROP_TYPE =="End terrace house")

RANKING_NEED_ENERGY <- data_select_gas %>% 
  mutate(octile_energy = ntile(energy, 8)) %>% 
  mutate(octile_area = ntile(energy_floor_area, 8)) %>% 
 #mutate(octileBounds_energy = cut2(energy, g=8))%>%
  mutate(octile_IMD = ntile(energy_IMD_ENG, 8)) %>% 
  mutate(octile_EE = ntile(energy_EE_BAND, 8)) %>% 
  #mutate(octileBounds_IMD = cut2(energy_IMD_ENG, g=8))%>%
  ungroup()
  

# Arrange by numeric values
RANKING_NEED_ENERGY <- RANKING_NEED_ENERGY %>% 
  arrange(octile_energy, energy)

write.csv(RANKING_NEED_ENERGY, file = "C:/NEED/Ranked_NEED_cleaned_2012.csv", row.names=FALSE)

# RANKING_NEED_ENERGY_IMD <- data_select_gas %>% 
#   # group_by(cluster) %>% 
#   mutate(octile_IMD = ntile(energy_IMD_ENG, 8)) %>% 
#   mutate(octileBounds_IMD = cut2(energy_IMD_ENG, g=8))%>%
#   ungroup()
# # Arrange by numeric values
# RANKING_NEED_ENERGY_IMD <- RANKING_NEED_ENERGY_IMD %>% 
#   arrange(octile_IMD,octileBounds_IMD, energy_IMD_ENG)
# 
# 
# select_energy <- RANKING_NEED_ENERGY %>%
#   select(HH_ID,
#          energy,
#          octile_energy,
#          octileBounds_energy)
# 
# 
#  select_energy_IMD <- RANKING_NEED_ENERGY_IMD %>%
#    select(HH_ID,
#           energy_IMD_ENG,
#           octile_IMD,
#           octileBounds_IMD)
# 
#  select_energy_IMD <- select_energy %>% 
#  inner_join(select_energy_IMD, "HH_ID") ## "HH-ID" is the common field to join


##### Logarithmic converisons #######

RANKING_NEED_ENERGY$energy <- log(RANKING_NEED_ENERGY$energy)
RANKING_NEED_ENERGY$energy_floor_area <- log(RANKING_NEED_ENERGY$energy_floor_area)
RANKING_NEED_ENERGY$energy_IMD_ENG <- log(RANKING_NEED_ENERGY$energy_IMD_ENG)
RANKING_NEED_ENERGY$energy_EE_BAND <- log(RANKING_NEED_ENERGY$energy_EE_BAND)


### Subset Octiles ####

select_energy_octile_1 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 1)
select_energy_octile_2 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 2)
select_energy_octile_3 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 3)
select_energy_octile_4 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 4)
select_energy_octile_5 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 5)
select_energy_octile_6 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 6)
select_energy_octile_7 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 7)
select_energy_octile_8 <- RANKING_NEED_ENERGY %>%
  subset(octile_energy == 8)

#Now, combine all dataframes from each octiles into one.  First make a new column in each.
RANKING_NEED_ENERGY$bind <- 'all'
select_energy_octile_1$bind <- 'octile_1'
select_energy_octile_2$bind <- 'octile_2'
select_energy_octile_3$bind <- 'octile_3'
select_energy_octile_4$bind <- 'octile_4'
select_energy_octile_5$bind <- 'octile_5'
select_energy_octile_6$bind <- 'octile_6'
select_energy_octile_7$bind <- 'octile_7'
select_energy_octile_8$bind <- 'octile_8'
# RANKING_NEED_ENERGY$bind_area <- RANKING_NEED_ENERGY$octile_area
# select_energy_octile$bind_area <- 'octile_1'


OCTILE_1_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_1)
OCTILE_2_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_2)
OCTILE_3_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_3)
OCTILE_4_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_4)
OCTILE_5_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_5)
OCTILE_6_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_6)
OCTILE_7_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_7)
OCTILE_8_ALL <- rbind(RANKING_NEED_ENERGY, select_energy_octile_8)


### Find quantile positions for each energy signature #######

quantile_energy <- quantile(RANKING_NEED_ENERGY$energy, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                 0.75, 0.875, 1))
quantile_energy_area <- quantile(RANKING_NEED_ENERGY$energy_floor_area, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                 0.75, 0.875, 1))
quantile_energy_IMD <- quantile(RANKING_NEED_ENERGY$energy_IMD_ENG, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                 0.75, 0.875, 1))
quantile_energy_EE <- quantile(RANKING_NEED_ENERGY$energy_EE_BAND, prob = c(0.125,0.25, 0.375, 0.5, 0.625,
                                                0.75, 0.875, 1))


index <- c(1:8)
LIST_ALL <-list(OCTILE_1_ALL,
            OCTILE_2_ALL,
            OCTILE_3_ALL,
            OCTILE_4_ALL,
            OCTILE_5_ALL,
            OCTILE_6_ALL,
            OCTILE_7_ALL,
            OCTILE_8_ALL)


### Energy

for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy",
                      paste("OCTILE_Energy_dens_", index[i], ".jpg", sep = ""))
  
OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
OCTILE_energy <-  assign(paste(paste("OCTILE",index[i],sep="_"),"energy",sep="_"),
  ggplot(OCTILE_ALL,aes(energy, fill = bind)) + 
  geom_density(alpha = 0.5, aes(y = ..count..)) +
  geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8)) +
  scale_x_continuous(breaks=quantile_energy) +
  scale_x_reverse() + 
  theme(legend.position = "none") +
  ggtitle(paste("Energy (2012) Octile", (index[i]))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=14, colour = "black")) + coord_flip())

outputname_F <- paste("plots/",gsub("[.]csv$","",index[i]),"_OCTILE_data.csv",sep ="")
print(outputname_F)
write.csv(OCTILE_ALL, file = outputname_F,row.names=FALSE)

ggsave(mypath, OCTILE_energy)
}



for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy",
                      paste("OCTILE_Energy_hist_", index[i], ".jpg", sep = ""))
  
OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
OCTILE_energy_hist <-  assign(paste(paste("OCTILE",index[i],sep="_"),"energy_hist",sep="_"),
  ggplot(OCTILE_ALL,aes(octile_energy, fill = bind)) + 
  geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
  scale_x_reverse() + 
  theme(legend.position = "none") +
  ggtitle(paste("Energy (2012) Octile", (index[i]))) +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=14, colour = "black")) + coord_flip())
ggsave(mypath, OCTILE_energy_hist)
}



#### Energy / Floor Area

for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy_floor_area",
                      paste("OCTILE_Energy_Floor_Area_dens_", index[i], ".jpg", sep = ""))
  
  OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  OCTILE_area <-  assign(paste(paste("OCTILE",index[i],sep="_"),"area",sep="_"),
                           ggplot(OCTILE_ALL,aes(energy_floor_area, fill = bind)) + 
                             geom_density(alpha = 0.5, aes(y = ..count..)) +
                             geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
                             scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8)) +
                             scale_x_continuous(breaks=quantile_energy) +
                            scale_x_reverse() + 
                             theme(legend.position = "none") +
                             ggtitle(paste("Energy / Floor Area (2012) Octile", (index[i]))) + 
                             theme(plot.title = element_text(lineheight=.8, face="bold",
                                                             size=14, colour = "black")) + coord_flip())

ggsave(mypath, OCTILE_area)
}
 


for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy_floor_area",
                    paste("OCTILE_Energy_Floor_Area_hist_", index[i], ".jpg", sep = ""))
  
  OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  OCTILE_area_hist <-  assign(paste(paste("OCTILE",index[i],sep="_"),"area_hist",sep="_"),
                                
                                ggplot(OCTILE_ALL,aes(octile_area, fill = bind)) + 
                                  geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
                                  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))+
                                  theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
                                scale_x_reverse() +   
                                theme(legend.position = "none") +
                                ggtitle(paste("Energy / Floor Area (2012) Octile", (index[i]))) + 
                                  theme(plot.title = element_text(lineheight=.8, face="bold",
                                    size=14, colour = "black")) + coord_flip())
ggsave(mypath, OCTILE_area_hist)
}


### Energy / IMD

for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy_IMD",
                      paste("OCTILE_Energy_IMD_dens_", index[i], ".jpg", sep = ""))
  
  OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  OCTILE_IMD <-  assign(paste(paste("OCTILE",index[i],sep="_"),"IMD",sep="_"),
                         
                         ggplot(OCTILE_ALL,aes(energy_IMD_ENG, fill = bind)) + 
                           geom_density(alpha = 0.5, aes(y = ..count..)) +
                           geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
                           scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8)) +
                           scale_x_continuous(breaks=quantile_energy) +
                          scale_x_reverse() +  
                          theme(legend.position = "none") +
                           ggtitle(paste("Energy / IMD (2012) Octile", (index[i]))) + 
                           theme(plot.title = element_text(lineheight=.8, face="bold",
                                                           size=14, colour = "black")) + coord_flip())
ggsave(mypath, OCTILE_IMD)
} 
  

for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy_IMD",
                      paste("OCTILE_Energy_IMD_hist_", index[i], ".jpg", sep = ""))
  
  OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  OCTILE_IMD_hist <-  assign(paste(paste("OCTILE",index[i],sep="_"),"IMD_hist",sep="_"),
                              
                              ggplot(OCTILE_ALL,aes(octile_IMD, fill = bind)) + 
                                geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
                                scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))+
                                theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
                               scale_x_reverse() +  
                               theme(legend.position = "none") +
                                ggtitle(paste("Energy / IMD (2012) Octile", (index[i]))) + 
                                theme(plot.title = element_text(lineheight=.8, face="bold",
                                                                size=14, colour = "black")) + coord_flip())
ggsave(mypath, OCTILE_IMD_hist)
}


### Energy / EE

for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy_EE",
                      paste("OCTILE_Energy_EE_dens_", index[i], ".jpg", sep = ""))
  
  OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  OCTILE_EE <-  assign(paste(paste("OCTILE",index[i],sep="_"),"EE",sep="_"),
                         
                         ggplot(OCTILE_ALL,aes(energy_EE_BAND, fill = bind)) + 
                           geom_density(alpha = 0.5, aes(y = ..count..)) +
                           geom_vline(xintercept=quantile_energy, color="red", linetype="dashed", size=0.5) +
                           scale_x_continuous(breaks=c(1, 2, 3, 4, 5,6,7,8)) +
                           scale_x_continuous(breaks=quantile_energy) +
                         scale_x_reverse() +   
                         theme(legend.position = "none") +
                           ggtitle(paste("Energy / EE (2012) Octile", (index[i]))) + 
                           theme(plot.title = element_text(lineheight=.8, face="bold",
                                                           size=14, colour = "black")) + coord_flip())
ggsave(mypath, OCTILE_EE)
}



for (i in 1:8) {
  mypath <- file.path("C:","NEED","plots", "octiles_plots", "energy_EE",
                      paste("OCTILE_Energy_EE_hist_", index[i], ".jpg", sep = ""))
  
  OCTILE_ALL <- assign(paste(paste("OCTILE",index[i],sep="_"),"ALL",sep="_"), 
                       as.data.frame(LIST_ALL[i]))
  OCTILE_EE_hist <-  assign(paste(paste("OCTILE",index[i],sep="_"),"EE_hist",sep="_"),
                             
                             ggplot(OCTILE_ALL,aes(octile_EE, fill = bind)) + 
                               geom_histogram(binwidth=0.3, alpha=.5, position="identity") +
                               scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))+
                               theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
                              scale_x_reverse() +  
                              theme(legend.position = "none") +
                               ggtitle(paste("Energy / EE (2012) Octile", (index[i]))) + 
                               theme(plot.title = element_text(lineheight=.8, face="bold",
                                                               size=14, colour = "black")) + coord_flip())
ggsave(mypath, OCTILE_EE_hist)
}


#### Araange plots ######

grid.arrange(OCTILE_1_energy, OCTILE_1_area, OCTILE_1_IMD, OCTILE_1_EE,
             OCTILE_2_energy, OCTILE_2_area, OCTILE_2_IMD, OCTILE_2_EE,
             OCTILE_3_energy, OCTILE_3_area, OCTILE_3_IMD, OCTILE_3_EE,
             OCTILE_4_energy, OCTILE_4_area, OCTILE_4_IMD, OCTILE_4_EE, 
             OCTILE_5_energy, OCTILE_5_area, OCTILE_5_IMD, OCTILE_5_EE, 
             OCTILE_6_energy, OCTILE_6_area, OCTILE_6_IMD, OCTILE_6_EE, 
             OCTILE_7_energy, OCTILE_7_area, OCTILE_7_IMD, OCTILE_7_EE, 
             OCTILE_8_energy, OCTILE_8_area, OCTILE_8_IMD, OCTILE_8_EE, 
             ncol=4, nrow =8,
#              top=textGrob("Flat (inc. maisonette)",gp=gpar(fontsize=20,font=3)))
 # top=textGrob("Detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Bungalow",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Semi-detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Mid terrace house",gp=gpar(fontsize=20,font=3)))
 top=textGrob("End terrace house",gp=gpar(fontsize=20,font=3)))


grid.arrange(OCTILE_1_energy_hist, OCTILE_1_area_hist, OCTILE_1_IMD_hist, OCTILE_1_EE_hist,
             OCTILE_2_energy_hist, OCTILE_2_area_hist, OCTILE_2_IMD_hist, OCTILE_2_EE_hist,
             OCTILE_3_energy_hist, OCTILE_3_area_hist, OCTILE_3_IMD_hist, OCTILE_3_EE_hist,
             OCTILE_4_energy_hist, OCTILE_4_area_hist, OCTILE_4_IMD_hist, OCTILE_4_EE_hist, 
             OCTILE_5_energy_hist, OCTILE_5_area_hist, OCTILE_5_IMD_hist, OCTILE_5_EE_hist, 
             OCTILE_6_energy_hist, OCTILE_6_area_hist, OCTILE_6_IMD_hist, OCTILE_6_EE_hist, 
             OCTILE_7_energy_hist, OCTILE_7_area_hist, OCTILE_7_IMD_hist, OCTILE_7_EE_hist, 
             OCTILE_8_energy_hist, OCTILE_8_area_hist, OCTILE_8_IMD_hist, OCTILE_8_EE_hist, 
             ncol=4, nrow =8,
           #  top=textGrob("Flat (inc. maisonette)",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Bungalow",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Semi-detached house",gp=gpar(fontsize=20,font=3)))
# top=textGrob("Mid terrace house",gp=gpar(fontsize=20,font=3)))
 top=textGrob("End terrace house",gp=gpar(fontsize=20,font=3)))


###################################################################################
###################################################################################
###################################################################################
###################################################################################

