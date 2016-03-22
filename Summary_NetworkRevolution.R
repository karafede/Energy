
library(threadr)
library(readr)
library(openair)
library(ggplot2)
library(plotly)
library(dplyr)

#### Load Residential Customer data ################################################

Basic_Profile_ID <- read_csv("C:/NetworkRevolution/TC1a/Electricity_data_TC1a.csv")
Basic_Profile_hour <- read_csv("C:/NetworkRevolution/TC1a/Electricity_data_TC1a_hour.csv")
Basic_Profile_ID$Sum_Electricity_kWh[Basic_Profile_ID$Sum_Electricity_kWh < 0] <- NA
Basic_Profile_hour$Sum_Electricity_kWh[Basic_Profile_hour$Sum_Electricity_kWh < 0] <- NA

### find ouliers
Outliers_Basic <- Basic_Profile_hour %>%
  arrange(-Sum_Electricity_kWh) 


### filter outliers
##### all OK !!!! ##########################################

Enhanced_Profile_ID <- read_csv("C:/NetworkRevolution/TC2a/Electricity_data_TC2a.csv") 
Enhanced_Profile_hour <- read_csv("C:/NetworkRevolution/TC2a/Electricity_data_TC2a_hour.csv") 
Enhanced_Profile_ID$Sum_Electricity_kWh[Enhanced_Profile_ID$Sum_Electricity_kWh < 0] <- NA
Enhanced_Profile_hour$Sum_Electricity_kWh[Enhanced_Profile_hour$Sum_Electricity_kWh < 0] <- NA

### find ouliers
Outliers_Enhanced <- Enhanced_Profile_hour %>%
    arrange(-Sum_Electricity_kWh) 

### filter outliers
Enhanced_Profile_ID <- Enhanced_Profile_ID[!Enhanced_Profile_ID$location_id == 10174,]
Enhanced_Profile_hour <- Enhanced_Profile_hour[!Enhanced_Profile_hour$location_id == 10174,]
Enhanced_Profile_ID <- Enhanced_Profile_ID[!Enhanced_Profile_ID$location_id == 10221,]
Enhanced_Profile_hour <- Enhanced_Profile_hour[!Enhanced_Profile_hour$location_id == 10221,]
Enhanced_Profile_ID <- Enhanced_Profile_ID[!Enhanced_Profile_ID$location_id == 10017,]
Enhanced_Profile_hour <- Enhanced_Profile_hour[!Enhanced_Profile_hour$location_id == 10017,]
Enhanced_Profile_ID <- Enhanced_Profile_ID[!Enhanced_Profile_ID$location_id == 10044,]
Enhanced_Profile_hour <- Enhanced_Profile_hour[!Enhanced_Profile_hour$location_id == 10044,]

#################################

Heat_Pumps_Profile_ID <- read_csv("C:/NetworkRevolution/TC3/Electricity_data_TC3.csv")
Heat_Pumps_Profile_hour <- read_csv("C:/NetworkRevolution/TC3/Electricity_data_TC3_hour.csv")
Heat_Pumps_Profile_ID$Sum_Electricity_kW[Heat_Pumps_Profile_ID$Sum_Electricity_kW < 0] <- NA
Heat_Pumps_Profile_hour$Sum_Electricity_kW[Heat_Pumps_Profile_hour$Sum_Electricity_kW < 0] <- NA

### find ouliers
Outliers_Heat_Pumps <- Heat_Pumps_Profile_hour %>%
  arrange(-Sum_Electricity_kW) 
### filter outliers
##### all OK !!!! ##########################################


Temperature_ID <- read_csv("C:/NetworkRevolution/TC3/Temperature_data_TC3.csv")
Temperature_hour <- read_csv("C:/NetworkRevolution/TC3/Temperature_data_TC3_hour.csv")
colnames(Temperature_ID)[10] <- "Temperature"
colnames(Temperature_hour)[11] <- "Temperature"

### find ouliers
Outliers_Temperature <- Temperature_hour %>%
  arrange(-Temperature)

### filter outliers
##### all OK !!!! ##########################################


SolarPV_Profile_ID <- read_csv("C:/NetworkRevolution/TC5/Electricity_data_TC5.csv")
SolarPV_Profile_hour <- read_csv("C:/NetworkRevolution/TC5/Electricity_data_TC5_hour.csv")

### find ouliers
Outliers_SolarPV <- SolarPV_Profile_hour %>%
  arrange(-Sum_Electricity_kW) 

### filter outliers
##### all OK !!!! ##########################################

Carbon_ID_TC5 <- read_csv("C:/NetworkRevolution/TC5/Carbon_data_TC5.csv")

############################################################

EV_Profile_ID <- read_csv("C:/NetworkRevolution/TC6/Electricity_data_TC6.csv") 
EV_Profile_hour <- read_csv("C:/NetworkRevolution/TC6/Electricity_data_TC6_hour.csv") 

### find ouliers
Outliers_EV_hour <- EV_Profile_hour %>%
  arrange(-Sum_Electricity_kWh) 

### filter outliers
##### all OK !!!! ##########################################


Smart_Profile_ID <- read_csv("C:/NetworkRevolution/TC9a/Electricity_data_TC9a.csv")
Smart_Profile_hour <- read_csv("C:/NetworkRevolution/TC9a/Electricity_data_TC9a_hour.csv")


### find ouliers
Outliers_Smart <- Smart_Profile_hour %>%
  arrange(-Sum_Electricity_kWh) 

### filter outliers
##### all OK !!!! ##########################################


SolarPV_Auto_ID <- read_csv("C:/NetworkRevolution/TC20_Auto/Electricity_data_TC20_Auto.csv")
SolarPV_Auto_hour <- read_csv("C:/NetworkRevolution/TC20_Auto/Electricity_data_TC20_Auto_hour.csv")

### find ouliers
Outliers_PVAuto <- SolarPV_Auto_hour %>%
  arrange(-Sum_Parameter) 

### filter outliers
##### all OK !!!! ##########################################


SolarPV_Manual_ID <- read_csv("C:/NetworkRevolution/TC20IHD/Electricity_data_TC20IHD.csv")
SolarPV_Manual_hour <- read_csv("C:/NetworkRevolution/TC20IHD/Electricity_data_TC20IHD_hour.csv")

### find ouliers
Outliers_PVManual <- SolarPV_Manual_hour %>%
  arrange(-Sum_Electricity_kW) 


Carbon_TC20HID <- read_csv("C:/NetworkRevolution/TC20IHD/Carbon_data_T20IHD.csv")

#############################################################################

SMEs_ID <- read_csv("C:/NetworkRevolution/TC1b/Electricity_data_TC1b_ID.csv")
SMEs_hour <- read_csv("C:/NetworkRevolution/TC1b/Electricity_data_TC1b_hour.csv")

### find ouliers
Outliers_SMEs <- SMEs_hour %>%
  arrange(-Sum_Electricity_kWh) 


### filter outliers
##### all OK !!!! ##########################################

################################################################################
################################################################################
############################## TC1a ############################################  
################################################################################
################################################################################

Basic_Profile_ID <- subset(Basic_Profile_ID, !is.na(mosaic_class))
Basic_Profile_hour <- subset(Basic_Profile_hour, !is.na(mosaic_class))

## Plot data 
Basic_Profile_hour %>% 
  filter(location_id == 1)  %>% ggplot(aes(hour, Sum_Electricity_kWh)) + geom_line()


jpeg('C:/NetworkRevolution/Plots/Cumulative_Basic_ID_TC1a.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

p <- ggplot(data = Basic_Profile_ID,
            aes(mosaic_class, Sum_Electricity_kWh, fill = mosaic_class)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("Cumulative Electr. Cons. (kWh)") +            # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
ggtitle("Electricity Consumption (Basic Profiles)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
p


par(oldpar)
dev.off()


########## TIME TREND #########################################################

jpeg('C:/NetworkRevolution/Plots/Time_Trend_all_Classes_Basic_Profiels(single)_TC1a.jpg',
     quality = 100, bg = "white", res = 200, width = 18, height = 11, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = Basic_Profile_hour, 
            aes(hour, Sum_Electricity_kWh, fill = hour)) +
  geom_bar(stat = "identity") + facet_wrap( ~ mosaic_class, ncol = 4) +  #scales="free_y"
  guides(fill=FALSE) +
  theme( strip.text = element_text(size = 15)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "red")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
   xlab("hour") +            # Set y-axis label
   theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
       axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  ggtitle("Time Trend of Consumption by class (Basic profile)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
q


par(oldpar)
dev.off()

# Sys.setenv("plotly_username" = "karafede")
# Sys.setenv("plotly_api_key" = "v516efgsn7")

# (gg <- ggplotly(q))
# plotly_POST(gg, filename = "home/prova_FK/Basic_Profile_Electricity_UK")

###################################################################################


jpeg('C:/NetworkRevolution/Plots/Time_Trend_all_Classes_TC1a.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = Basic_Profile_hour, 
        aes(hour, Sum_Electricity_kWh, fill = hour)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  xlab("hour") +            # Set y-axis label
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
ggtitle("Time Trend of Consumption (all classes)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
q

par(oldpar)
dev.off()



#########################################################################

Alpha_Territory <-Basic_Profile_hour %>% 
  filter(mosaic_class == "Alpha Territory")

Ex_Council_Community <-Basic_Profile_hour %>% 
  filter(mosaic_class == "Ex-Council Community")

Upper_Floor_Living <-Basic_Profile_hour %>% 
  filter(mosaic_class == "UPPER FLOOR Living")

Active_Retirement <-Basic_Profile_hour %>% 
  filter(mosaic_class == "Active Retirement")

Suburban_Mindsets <-Basic_Profile_hour %>% 
  filter(mosaic_class == "Suburban Mindsets")

New_Homemakers <-Basic_Profile_hour %>% 
  filter(mosaic_class == "NEW Homemakers")

Terraced_Melting_Pot <-Basic_Profile_hour %>% 
  filter(mosaic_class == "Terraced Melting Pot")

################################################################################


jpeg('C:/NetworkRevolution/Plots/Ex_Council_Community_hour_TC1a.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


q <- ggplot(data = Ex_Council_Community, 
            aes(hour, Sum_Electricity_kWh, fill = hour)) +
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  xlab("hour") +            # Set y-axis label
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  ggtitle("Ex Council Community consumption") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
q

par(oldpar)
dev.off()

############################

#### Diplay time when maximum consumption was measured ######################

Basic_Profile_max <- Basic_Profile_hour %>% 
  group_by(location_id) %>% 
  arrange(-Sum_Electricity_kWh) %>%  ### from the biggest to the smaller (decreasing order)
  slice(1) %>%
  ungroup()
colnames(Basic_Profile_max)[10] <- "max_hour"


jpeg('C:/NetworkRevolution/Plots/Max_Consumption_all_classes_TC1a.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)  

  p <- ggplot(data = Basic_Profile_max,
              aes(max_hour, Sum_Electricity_kWh, fill = max_hour)) + guides(fill=FALSE) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr. Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Maximum Electricity Consumption (all classes)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  p
  
  par(oldpar)
  dev.off()
  
  
#############################################

 jpeg('C:/NetworkRevolution/Plots/Max_Consumption_by_class_TC1a.jpg',
       quality = 100, bg = "white", res = 200, width = 18, height = 11, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)  
  
  p <- ggplot(data = Basic_Profile_max,
              aes(max_hour, Sum_Electricity_kWh, fill = max_hour)) + guides(fill=FALSE) +
    geom_bar(stat = "identity") + facet_wrap( ~ mosaic_class, ncol = 4, scales="free_y") +
    guides(fill=FALSE) +
    theme( strip.text = element_text(size = 15)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr. Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Maximum Electricity Consumption by class") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  par(oldpar)
  dev.off()
  
################################################################################
################################################################################
################################################################################
############################## TC2a ############################################  
################################################################################
################################################################################ 

### Filter Total Property and Upstair Lights ##########################

Enhanced_Profile_ID <- subset(Enhanced_Profile_ID, !is.na(measurement_description))
Enhanced_Profile_hour <- subset(Enhanced_Profile_hour, !is.na(measurement_description))

Enhanced_Profile_ID <- subset(Enhanced_Profile_ID, !is.na(Sum_Electricity_kWh))
Enhanced_Profile_hour <- subset(Enhanced_Profile_hour, !is.na(Sum_Electricity_kWh))

Enhanced_Profile_ID <- Enhanced_Profile_ID[!Enhanced_Profile_ID$measurement_description == "Total Property",]
  
  jpeg('C:/NetworkRevolution/Plots/Cumulative_Enhanced_Filtered_ID_TC2a.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = Enhanced_Profile_ID,
              aes(measurement_description, Sum_Electricity_kWh, fill = measurement_description)) + 
    geom_bar(stat = "identity") + guides(fill=FALSE) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr. Cons. (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Electricity Consumption by Utility  (Enhanced Profiles)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  ########################################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_all_Utilities_TC2a.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = Enhanced_Profile_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + guides(fill=FALSE) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend of Consumption (all utilities)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  q
  
  par(oldpar)
  dev.off()
  
  
  ################################
  
jpeg('C:/NetworkRevolution/Plots/Time_Trend_by_Utility_TC2a.jpg',
       quality = 100, bg = "white", res = 200, width = 18, height = 11, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = Enhanced_Profile_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + facet_wrap( ~ measurement_description, ncol = 4, scales="free_y") +
    guides(fill=FALSE) +
    theme( strip.text = element_text(size = 15)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend of Consumption by utility") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  q
  
  par(oldpar)
  dev.off()
  
  
  
  #########################################################################
  
  Total_Property <-Enhanced_Profile_hour %>% 
    filter(measurement_description == "Total Property")
  
  Upstairs_Lights <-Enhanced_Profile_hour %>% 
    filter(measurement_description == "Upstairs lights")
    
  Electric_Heater <-Enhanced_Profile_hour %>% 
    filter(measurement_description == "Electric heater")
  
  Cooker <- Enhanced_Profile_hour %>% 
    filter(measurement_description == "Cooker")
  
  Washing_Machine <- Enhanced_Profile_hour %>% 
    filter(measurement_description == "Washing Machine")
  
 ########################################################################## 
  
  jpeg('C:/NetworkRevolution/Plots/Total_Property_hour_TC2a.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = Total_Property, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + guides(fill=FALSE) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Total Property consumption") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  q
  
  par(oldpar)
  dev.off()
  
  
  #############################

  #### Diplay time when maximum consumption was measured ######################

  Enhanced_Profile_max <- Enhanced_Profile_hour %>% 
    group_by(location_id) %>% 
    arrange(-Sum_Electricity_kWh) %>%  
    slice(1) %>%
    ungroup()
  colnames(Enhanced_Profile_max)[10] <- "max_hour"

  
  jpeg('C:/NetworkRevolution/Plots/Max_Consumption_all_utilities_TC2a.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)  
  
  p <- ggplot(data = Enhanced_Profile_max,
              aes(max_hour, Sum_Electricity_kWh, fill = max_hour)) +
    geom_bar(stat = "identity") + guides(fill=FALSE) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr. Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Maximum Electricity Consumption (all utilities)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  par(oldpar)
  dev.off()
  
 

  ################################################################################
  ################################################################################
  ############################## TC3 ############################################  
  ################################################################################
  ################################################################################
  
  jpeg('C:/NetworkRevolution/Plots/Cumulative_Heat_Pumps_ID_TC3.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = Heat_Pumps_Profile_ID,
              aes(measurement_description, Sum_Electricity_kW, fill = measurement_description)) + 
    geom_bar(stat = "identity")  +
     theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power Consumption & Import (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Power import and Power consumption") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 15))
  p
  
  par(oldpar)
  dev.off()
  
  #############################################################################
  
  Heat_Pump_Power <- Heat_Pumps_Profile_hour %>% 
    filter(measurement_description == "heat pump power consumption")
  
  Whole_home_Power <-Heat_Pumps_Profile_hour %>% 
    filter(measurement_description == "whole home power import")
  
  ###########################################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_Heat_Pumps&Power_TC3.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  q <- ggplot(data = Heat_Pumps_Profile_hour, 
              aes(hour, Sum_Electricity_kW, fill = hour)) +
    geom_bar(stat = "identity") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend for Heat Pumps consumption & Home power import") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  q
  
  par(oldpar)
  dev.off()
  
  ############################################################################
  
  #### Diplay time when maximum consumption was measured ######################
  
  Power_max <- Heat_Pumps_Profile_hour  %>% 
    group_by(location_id,
             measurement_description) %>% 
    arrange(-Sum_Electricity_kW) %>%  
    slice(1) %>%
    ungroup()
  colnames(Power_max)[10] <- "max_hour"
  
  
  jpeg('C:/NetworkRevolution/Plots/Maximum_Heat_Pumps&Power_TC3.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)  
  
  p <- ggplot(data = Power_max, 
              aes(max_hour, Sum_Electricity_kW, fill = max_hour)) +
    geom_bar(stat = "identity") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Maximum Heat Pumps consumption & Home power import") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  
  par(oldpar)
  dev.off()
  
  ####### Temperature data ###################################################
  ############################################################################
  
  jpeg('C:/NetworkRevolution/Plots/Average_Temperature_Heat_Pumps_ID_TC3.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = Temperature_ID,
              aes(x = measurement_description, y = Temperature, fill = measurement_description)) + 
    stat_summary(fun.y=mean, geom="bar")  +
    # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    # theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Average Temperature (Celsius)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Average External & Zone 1 Temperature") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  par(oldpar)
  dev.off()
  
  #############################################################################
  
  External_Temp <- Temperature_hour %>% 
    filter(measurement_description == "External temperature")
  
  Zone_1_Temp <-Temperature_hour %>% 
    filter(measurement_description == "Zone 1 temperature")
  
  ############################################################################
  
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_Temperature_TC3.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  q <- ggplot(data = Temperature_hour, 
              aes(x = hour, y = Temperature, fill = hour)) +
    stat_summary(fun.y=mean, geom="bar") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Temperature (Celsius)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend External and Zone 1 temperature") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  q
  
  par(oldpar)
  dev.off()
  
  
  #### Diplay time when maximum consumption was measured ######################
  
  Temp_max <- Temperature_hour  %>% 
    group_by(location_id,
             measurement_description) %>% 
    arrange(-Temperature) %>%  
    slice(1) %>%
    ungroup()
  colnames(Temp_max)[10] <- "max_hour"
  
#   jpeg('C:/NetworkRevolution/Plots/Max_Temperature_TC3.jpg',
#        quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
#   par(mar=c(4, 10, 9, 2) + 0.3)
#   oldpar <- par(las=1)
#   
#   q <- ggplot(data = Temp_max, 
#               aes(x = max_hour, y = Temperature, fill = max_hour)) +
#     stat_summary(fun.y=mean, geom="bar") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
#     theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
#     theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
#     theme(axis.title.x = element_blank()) +                  # Remove x-axis label
#     ylab("Temperature (Celsius)") +            # Set y-axis label
#     theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
#           axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
#     xlab("hour") +            # Set y-axis label
#     theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
#           axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
#     ggtitle("Maximum External and Zone 1 temperature") + 
#     theme(plot.title = element_text(lineheight=.8, face="bold"))
#   q
#   
#   par(oldpar)
#   dev.off()
  
  
  ################################################################################
  ################################################################################
  ############################## TC5 ############################################  
  ################################################################################
  ################################################################################
  
  SolarPV_Profile_ID <- subset(SolarPV_Profile_ID, !is.na(measurement_description))
  SolarPV_Profile_ID <- subset(SolarPV_Profile_ID, !is.na(Sum_Electricity_kW))
  
  jpeg('C:/NetworkRevolution/Plots/Cumulative_SolarPV_TC5.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = SolarPV_Profile_ID,
              aes(measurement_description, Sum_Electricity_kW, fill = measurement_description)) + 
    geom_bar(stat = "identity", position = "identity")  +
    theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power Consumption & Import (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Solar Power & Whole home import Power consumption") + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    geom_hline(yintercept = 0)
    p
  
  
  par(oldpar)
  dev.off()
  
  ######### TIME TREND  #########################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_Solar_Power_TC5.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  q <- ggplot(data = SolarPV_Profile_hour, 
              aes(hour, Sum_Electricity_kW, fill = hour)) +
    geom_bar(stat = "identity", position = "identity") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend for Solar and Whole Home power import") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  geom_hline(yintercept = 0)
  q
  
  par(oldpar)
  dev.off()
  

  ####### CARBON DATA data for Solar Panel users #############################
  ############################################################################
  
  
  jpeg('C:/NetworkRevolution/Plots/System_Size_Peak_Power_SolarPV_TC5.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = Carbon_ID_TC5,
              aes(measurement_description, System_Size_kW_peak, fill = measurement_description)) + 
    geom_bar(stat = "identity")  + facet_grid(. ~ System_Size_kW_peak) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
     theme(axis.text.x=element_text(angle=90,hjust=1,vjust=2)) +
     theme(axis.text.x=element_text(size=20,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Peak Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Peak Power of solar PV panels") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  ################################################################################
  ################################################################################
  ############################## TC6 #############################################  
  ################################################################################
  ################################################################################
  
  
  EV_Profile_ID <- subset(EV_Profile_ID, !is.na(Sum_Electricity_kWh))
  EV_Profile_hour <- subset(EV_Profile_hour, !is.na(Sum_Electricity_kWh))
  
  jpeg('C:/NetworkRevolution/Plots/Electric_Vehicles_User_Power_Consumption_TC6.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = EV_Profile_ID,
              aes(measurement_description, Sum_Electricity_kWh, fill = measurement_description)) + 
    geom_bar(stat = "identity")  +
     theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
     theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Electric Vehicles Power User Consumption") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  ######### TIME TREND  #########################################################  
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_Electric_Vehicles_User_Consumption_TC6.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  q <- ggplot(data = EV_Profile_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend of Electric Vehicles Power Consumption") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  q
  
  par(oldpar)
  dev.off()
  
  
  ################################################################################
  ################################################################################
  ############################## TC9a ############################################  
  ################################################################################
  ################################################################################
  
  Smart_Profile_ID <- subset(Smart_Profile_ID, !is.na(mosaic_class))
  Smart_Profile_hour <- subset(Smart_Profile_hour, !is.na(mosaic_class))
  
  
  jpeg('C:/NetworkRevolution/Plots/Cumulative_Smart_ID_TC9a.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = Smart_Profile_ID,
              aes(mosaic_class, Sum_Electricity_kWh, fill = mosaic_class)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr. Cons. (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Electricity Consumption (Smart Profiles)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  ########## TIME TREND #########################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_all_Classes_Smart_Profiles(single)_TC9a.jpg',
       quality = 100, bg = "white", res = 200, width = 18, height = 11, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = Smart_Profile_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + facet_wrap( ~ mosaic_class, ncol = 4) + ##scales="free_y"
    guides(fill=FALSE) + 
    theme( strip.text = element_text(size = 15)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "red")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend of Consumption by class (Smart Profiles)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  q
  
  
  par(oldpar)
  dev.off()
  
  
  ################################################################################
  ################################################################################
  ############################## TC20_Auto #############################################  
  ################################################################################
  ################################################################################
  
  
  SolarPV_Auto_ID <- subset(SolarPV_Auto_ID, !is.na(Sum_Parameter))

  #### Rename measurement description ##################################
  
  SolarPV_Auto_ID$measurement_description <- ifelse(grepl("Demand current",SolarPV_Auto_ID$measurement_description , ignore.case = TRUE), 
                                   "Demand current [A]", SolarPV_Auto_ID$measurement_description)
  SolarPV_Auto_ID$measurement_description <- ifelse(grepl("Maximum current exported",SolarPV_Auto_ID$measurement_description , ignore.case = TRUE), 
                                                    "Maximum current exported [A]", SolarPV_Auto_ID$measurement_description)
  SolarPV_Auto_ID$measurement_description <- ifelse(grepl("Supply voltage",SolarPV_Auto_ID$measurement_description , ignore.case = TRUE), 
                                                    "Supply voltage [V]", SolarPV_Auto_ID$measurement_description)
  SolarPV_Auto_ID$measurement_description <- ifelse(grepl("Photovoltaic meter",SolarPV_Auto_ID$measurement_description , ignore.case = TRUE), 
                                                    "Photovoltaic meter [kWh]", SolarPV_Auto_ID$measurement_description)
  
  
  SolarPV_Auto_hour <- subset(SolarPV_Auto_hour, !is.na(Sum_Parameter))
  
  SolarPV_Auto_hour$measurement_description <- ifelse(grepl("Demand current",SolarPV_Auto_hour$measurement_description , ignore.case = TRUE), 
                                                    "Demand current [A]", SolarPV_Auto_hour$measurement_description)
  SolarPV_Auto_hour$measurement_description <- ifelse(grepl("Maximum current exported",SolarPV_Auto_hour$measurement_description , ignore.case = TRUE), 
                                                    "Maximum current exported [A]", SolarPV_Auto_hour$measurement_description)
  SolarPV_Auto_hour$measurement_description <- ifelse(grepl("Supply voltage",SolarPV_Auto_hour$measurement_description , ignore.case = TRUE), 
                                                    "Supply voltage [V]", SolarPV_Auto_hour$measurement_description)
  SolarPV_Auto_hour$measurement_description <- ifelse(grepl("Photovoltaic meter",SolarPV_Auto_hour$measurement_description , ignore.case = TRUE), 
                                                    "Photovoltaic meter [kWh]", SolarPV_Auto_hour$measurement_description)
  
  
  jpeg('C:/NetworkRevolution/Plots/Solar_PV_auto_hot_water_charging_TC20_Auto.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  p <- ggplot(data = SolarPV_Auto_ID,
              aes(measurement_description, Sum_Parameter, fill = measurement_description)) + 
    geom_bar(stat = "identity", position = "identity")  +  
    # facet_wrap( ~ measurement_description, ncol = 4) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=20,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Consumption") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Consumption from solar PV with automatic hot water charging") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  
  ######### TIME TREND  #########################################################  
  
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_Solar_PV_auto_hot_water_charging_TC20_Auto.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  q <- ggplot(data = SolarPV_Auto_hour, 
              aes(hour, Sum_Parameter, fill = hour)) +
    geom_bar(stat = "identity") +
    facet_wrap( ~ measurement_description, ncol = 2, scales="free_y") + guides(fill=FALSE) + 
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Consumption") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Consumption from solar PV with automatic hot water charging") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  q
  
  par(oldpar)
  dev.off()
  
  
  #################################################################################
  ################################################################################
  ############################## TC20IHD ############################################  
  ################################################################################
  ################################################################################
  
 
  jpeg('C:/NetworkRevolution/Plots/Cumulative_SolarPV_manual_control_TC20IHD.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = SolarPV_Manual_ID,
              aes(measurement_description, Sum_Electricity_kW, fill = measurement_description)) + 
    geom_bar(stat = "identity", position = "identity")  +
    theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power Consumption & Import (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Solar Power & Whole home import Power (Manual Control)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 15)) +
  geom_hline(yintercept = 0)
  p
  
  
  par(oldpar)
  dev.off()
  
  
  ######### TIME TREND  #########################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_Solar_PV_manual_control_TC20IHD.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  q <- ggplot(data = SolarPV_Manual_hour, 
              aes(hour, Sum_Electricity_kW, fill = hour)) +
    geom_bar(stat = "identity", position = "identity") + facet_grid(. ~ measurement_description) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Time Trend for Solar and Whole Home power import(Manual Control)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  geom_hline(yintercept = 0)
  q
  
  par(oldpar)
  dev.off()
  

  
  ####### cARBON DATA data for Solar Panel users #############################
  ############################################################################
  
  
  jpeg('C:/NetworkRevolution/Plots/Peak_Power_SolarPV_Manual_Control_TC20IHD.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = Carbon_TC20HID,
              aes(measurement_description, System_Size_kW_peak, fill = measurement_description)) + 
    geom_bar(stat = "identity")  + facet_grid(. ~ System_Size_kW_peak) + guides(fill=FALSE) +
    theme( strip.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=13,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Peak Power (kW)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Peak Power of solar PV panels (Manual Control)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  
  ################################################################################
  ################################################################################
  ############################## TC1b ############################################  
  ################################################################################
  ################################################################################
  
SMEs_ID <- subset(SMEs_ID, !is.na(sub_group))
SMEs_hour <- subset(SMEs_hour, !is.na(sub_group))
SMEs_ID <- subset(SMEs_ID, !is.na(size))
SMEs_hour <- subset(SMEs_hour, !is.na(size))
SMEs_ID <- subset(SMEs_ID, !is.na(Sum_Electricity_kWh))
SMEs_hour <- subset(SMEs_hour, !is.na(Sum_Electricity_kWh))
  
  ## Plot data 
SMEs_hour %>% 
  filter(location_id == 100736)  %>% ggplot(aes(hour, Sum_Electricity_kWh)) + geom_line()
  
  
  jpeg('C:/NetworkRevolution/Plots/Cumulative_Electricity_SMEs_ID_TC1b.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  p <- ggplot(data = SMEs_ID,
              aes(sector, Sum_Electricity_kWh, fill = sector)) +
    geom_bar(stat = "identity") + 
    facet_wrap( ~ size, scales="free_y") + guides(fill=FALSE) + 
    theme( strip.text = element_text(size = 15)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr. Cons. (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Electricity Consumption (SMEs)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  p
  
  
  par(oldpar)
  dev.off()
  
  
  ########## TIME TREND #########################################################
  
  
  jpeg('C:/NetworkRevolution/Plots/Total_SMEs_hour_TC1b.jpg',
       quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = SMEs_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + guides(fill=FALSE) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=13),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=13),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
    ggtitle("Total Property consumption for SMEs") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  q
  
  par(oldpar)
  dev.off()
  
  
  ###############################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_by_Sector_SMEs_TC1b.jpg',
       quality = 100, bg = "white", res = 200, width = 18, height = 11, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = SMEs_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + facet_wrap( ~ sector, ncol = 2) +
    guides(fill=FALSE) + 
    theme( strip.text = element_text(size = 30)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=15,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=15),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=15)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=20)) +
    ggtitle("Time Trend of Consumption by Sector (SMEs)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30))
  q
  
  
  par(oldpar)
  dev.off()
  
  
  #####################################################################
  
  jpeg('C:/NetworkRevolution/Plots/Time_Trend_by_Size_SMEs_TC1b.jpg',
       quality = 100, bg = "white", res = 200, width = 18, height = 11, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  q <- ggplot(data = SMEs_hour, 
              aes(hour, Sum_Electricity_kWh, fill = hour)) +
    geom_bar(stat = "identity") + facet_wrap( ~ size, ncol = 4) +
    guides(fill=FALSE) + 
    theme( strip.text = element_text(size = 30)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("Cumulative Electr.Consumption (kWh)") +            # Set y-axis label
    theme(axis.title.y = element_text(face="bold", colour="#990000", size=15),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=15)) +
    xlab("hour") +            # Set y-axis label
    theme(axis.title.x = element_text(face="bold", colour="#990000", size=15),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=20)) +
    ggtitle("Time Trend of Consumption by Size (SMEs)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 25))
  q
  
  
  par(oldpar)
  dev.off()
  