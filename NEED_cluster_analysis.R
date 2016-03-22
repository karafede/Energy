# Set-up ---------------------------
# Load packages
library(threadr)
library(ggplot2)
library(dplyr)
library(tidyr)
library (devtools)
library(readxl)
library(Hmisc)

# devtools::install_github("skgrange/threadr")
# Set global options
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

#### Rename regions ##############################################################

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

 

 

# data_energy_gas$inv_FLOOR_AREA_BAND <- 1/data_energy_gas$FLOOR_AREA_BAND
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
data_select_gas$gas_floor_area <- (data_select_gas$Gcons2012)/(data_select_gas$FLOOR_AREA_BAND)
data_select_gas$electricity_floor_area <- (data_select_gas$Econs2012)/(data_select_gas$FLOOR_AREA_BAND)
data_select_gas$energy_inv_IMD_ENG <- (data_select_gas$energy)/(1/(data_select_gas$IMD_ENG))
data_select_gas$energy_IMD_ENG <- (data_select_gas$energy)/(data_select_gas$IMD_ENG)
data_select_gas$energy_EE_BAND <- (data_select_gas$energy)/(data_select_gas$EE_BAND)


data_select <- data_select_gas %>% 
  select(HH_ID,
         REGION,
         # Gcons2012,
         # Econs2012,
         FLOOR_AREA_BAND,
        PROP_TYPE,
         EE_BAND,
        inv_IMD_ENG) %>%
na.omit()

# Make REGION a factor too
data_select <- data_select %>% 
  mutate(REGION = ifelse(REGION == "North East", 1, REGION), 
         REGION = ifelse(REGION == "North West", 2, REGION),
         REGION = ifelse(REGION == "Yorkshire and The Humber", 3, REGION),
         REGION = ifelse(REGION == "East Midlands", 4, REGION),
         REGION = ifelse(REGION == "West Midlands", 5, REGION),
         REGION = ifelse(REGION == "East of England", 6, REGION),
         REGION = ifelse(REGION == "London", 7, REGION),
         REGION = ifelse(REGION == "South East", 8, REGION),
         REGION = ifelse(REGION == "South West", 9, REGION))
data_select$REGION <- as.numeric(data_select$REGION)

# Make PROP_TYPE a factor too
data_select <- data_select %>% 
  mutate(PROP_TYPE = ifelse(PROP_TYPE == "Detached house", 1, PROP_TYPE), 
         PROP_TYPE = ifelse(PROP_TYPE == "Semi-detached house", 2, PROP_TYPE),
         PROP_TYPE = ifelse(PROP_TYPE == "End terrace house", 3, PROP_TYPE),
         PROP_TYPE = ifelse(PROP_TYPE == "Mid terrace house", 4, PROP_TYPE),
         PROP_TYPE = ifelse(PROP_TYPE == "Bungalow", 5, PROP_TYPE),
         PROP_TYPE = ifelse(PROP_TYPE == "Flat (inc. maisonette)", 6, PROP_TYPE))
data_select$PROP_TYPE <- as.numeric(data_select$PROP_TYPE)


# Plots
summary_REGION <- data_select_gas %>% 
  group_by(REGION) %>% 
  summarise(gas = mean(Gcons2012, na.rm = TRUE), 
            electricity = mean(Econs2012, na.rm = TRUE),
            energy = mean(energy, na.rm = TRUE))




jpeg('C:/NEED/plots/Gas_summary.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

summary_REGION %>% 
  ggplot(aes(REGION, gas, fill = REGION)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Gas Consumption (2012)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Electricity_summary.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

summary_REGION %>% 
  ggplot(aes(REGION, electricity, fill = REGION)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Electricity Consumption (2012)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Energy_summary.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

summary_REGION %>% 
  ggplot(aes(REGION, energy, fill = REGION)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
ggtitle("Total Energy Consumption (2012)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
par(oldpar)
dev.off()



########## Ranking ###########################################

data_select_gas <- data_select_gas %>%
  subset(PROP_TYPE =="Flat (inc. maisonette)")

RANKING_NEED_ENERGY <- data_select_gas %>% 
  # group_by(cluster) %>% 
  mutate(octile_energy = ntile(energy, 8)) %>% 
 mutate(octileBounds_energy = cut2(energy, g=8))%>%
  ungroup()
  

# Arrange by numeric values
RANKING_NEED_ENERGY <- RANKING_NEED_ENERGY %>% 
  arrange(octile_energy,octileBounds_energy, energy)


RANKING_NEED_ENERGY_IMD <- data_select_gas %>% 
  # group_by(cluster) %>% 
  mutate(octile_IMD = ntile(energy_IMD_ENG, 8)) %>% 
  mutate(octileBounds_IMD = cut2(energy_IMD_ENG, g=8))%>%
  ungroup()
# Arrange by numeric values
RANKING_NEED_ENERGY_IMD <- RANKING_NEED_ENERGY_IMD %>% 
  arrange(octile_IMD,octileBounds_IMD, energy_IMD_ENG)


select_energy <- RANKING_NEED_ENERGY %>%
  select(HH_ID,
         energy,
         octile_energy,
         octileBounds_energy)


 select_energy_IMD <- RANKING_NEED_ENERGY_IMD %>%
   select(HH_ID,
          energy_IMD_ENG,
          octile_IMD,
          octileBounds_IMD)
# colnames(select_energy_IMD) <- c("HH_ID_IMD", "energy_IMD_ENG",
#                                  "octile_IMD","octileBounds_IMD")

 
# select_data <- cbind(select_energy, select_energy_IMD)

 select_energy_IMD <- select_energy %>% 
 inner_join(select_energy_IMD, "HH_ID") ## "HH-ID" is the common field to join

  select_energy_octile <- select_energy_IMD %>%
    subset(octile_energy == 7)
 

select_energy_octile %>% 
  ggplot(aes(energy, fill = as.factor(octile_energy), 
             colour = as.factor(octile_energy))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Energy 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

select_energy_octile %>% 
     ggplot(aes(energy, fill=octile_energy))+ 
     geom_histogram(binwidth = 100)
  

select_energy_octile %>% 
  ggplot(aes(energy_IMD_ENG, fill = as.factor(octile_energy), 
             colour = as.factor(octile_energy))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Energy / IMD (2012)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

select_energy_octile %>% 
  ggplot(aes(energy_IMD_ENG, fill=octile_energy))+ 
  geom_histogram(binwidth = 100)


RANKING_NEED_ENERGY %>%  ### without clustering
  ggplot(aes(energy, fill = as.factor(octile_energy), 
             colour = as.factor(octile_energy))) + 
  geom_density(alpha = .5) +
  ggtitle("Energy 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))






# write.csv(DATA_FINAL_ALL, file = "DATA_FINAL_ALL.csv", row.names=TRUE)

# Counts of flats per cluster quartile
RANKING_NEED_ENERGY %>%  
  group_by(octile) %>% 
  summarise(n = n())


# ##Get HH_ID with high consumption for their cluster
# data_high <- TOT_ENERGY %>% 
#   filter(octile == 4)

# View(a[, 50:101])

RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==1] <- "LLL"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==2] <- "LL"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==3] <- "L"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==4] <- "M"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==5] <- "MM"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==6] <- "H"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==7] <- "HH"
RANKING_NEED_ENERGY$octile[RANKING_NEED_ENERGY$octile==8] <- "HHH"

### subset....
#### filter...


write.csv(RANKING_NEED_ENERGY, file = "RANKING_NEED_ENERGY.csv", row.names=TRUE)

Summary_Energy <- RANKING_NEED_ENERGY %>%  
  group_by(octile) %>% 
  summarise(n = n())


write.csv(Summary_Energy, file = "Summary_Energy.csv", row.names=TRUE)


###############################################################################







# data_select %>% 
#   ggplot(aes(HH_ID, Gcons2012)) + 
#   geom_point(colour = "darkred", size = 4) + 
#   geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))


# Store unique identifiers
rownames <- data_select$HH_ID

# Give data frame unique row names
row.names(data_select) <- rownames
data_select$HH_ID <- NULL

# Standardise variables (rescale data based on the meand and Standard deviation)
data_select_standard <- standardise(data_select)
# data_select_standard <- data.frame(scale(data_select))
 

# data_select_standard$HH_ID <- data_select_gas$HH_ID 
# data_select_standard %>% 
#    ggplot(aes(HH_ID, Gcons2012)) + 
#    geom_point(colour = "darkred", size = 4) +
#    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))

############### PRINCIPAL COMPONENT ANALYSIS ################

NEED.pca <- prcomp(data_select_standard,
                  center = TRUE,
                  scale. = TRUE)

plot(NEED.pca, type = "l")
summary(NEED.pca) ### cumulative
plot(NEED.pca)

Prop_Var <- as.data.frame(NEED.pca$sdev^2/sum(NEED.pca$sdev^2)*100)


jpeg('C:/NEED/plots/PCA_NEED.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

plot(NEED.pca)

par(oldpar)
dev.off()


################ CLUSTER ANALYSIS #############################
 
# ### Sample_data
# x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
#            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
#  colnames(x) <- c("x", "y")
#  (cl <- kmeans(x, 2))
#  cl <- kmeans(x, 4)
#  plot(x, col = cl$cluster)
#  points(cl$centers, col = 1:2, pch = 8, cex = 2)

kms <- kmeans(data_select_standard, centers = 5) ### number of cluster = 4
class(kms)
cluster <- kms$cluster


# Give observations cluster variable 
data_post_cluster <- data_select %>% 
  mutate(HH_ID = rownames,         ### add flat name (new column)
         cluster = unname(cluster)) %>%   ### add cluste column
  arrange(cluster)

# Join cluster group to data
# Select
data_post_cluster <- data_post_cluster %>% 
  select(HH_ID, cluster)

# Join

data_post_cluster <- data_select_gas %>% 
  inner_join(data_post_cluster, "HH_ID") ## "HH-ID" is the common field to join

write.csv(data_post_cluster, file = "data_post_cluster.csv", row.names=TRUE)


cluster_1 <- subset(data_post_cluster, cluster == 1)
cluster_2 <- subset(data_post_cluster, cluster == 2)
cluster_3 <- subset(data_post_cluster, cluster == 3)
cluster_4 <- subset(data_post_cluster, cluster == 4)
cluster_5 <- subset(data_post_cluster, cluster == 5)


######## Rename Cluster identification ##################################

data_post_cluster_NAMES <- data_post_cluster
data_post_cluster_NAMES$PROP_AGE <- ifelse(grepl("101", data_post_cluster_NAMES$PROP_AGE, ignore.case = TRUE), 
                                 "before 1930", data_post_cluster_NAMES$PROP_AGE)
data_post_cluster_NAMES$PROP_AGE <- ifelse(grepl("102", data_post_cluster_NAMES$PROP_AGE, ignore.case = TRUE), 
                                           "1930-1949", data_post_cluster_NAMES$PROP_AGE)
data_post_cluster_NAMES$PROP_AGE <- ifelse(grepl("103", data_post_cluster_NAMES$PROP_AGE, ignore.case = TRUE), 
                                           "1950-1966", data_post_cluster_NAMES$PROP_AGE)
data_post_cluster_NAMES$PROP_AGE <- ifelse(grepl("104", data_post_cluster_NAMES$PROP_AGE, ignore.case = TRUE), 
                                           "1967-1982", data_post_cluster_NAMES$PROP_AGE)
data_post_cluster_NAMES$PROP_AGE <- ifelse(grepl("105", data_post_cluster_NAMES$PROP_AGE, ignore.case = TRUE), 
                                           "1983-1995", data_post_cluster_NAMES$PROP_AGE)
data_post_cluster_NAMES$PROP_AGE <- ifelse(grepl("106", data_post_cluster_NAMES$PROP_AGE, ignore.case = TRUE), 
                                           "1996 onwards", data_post_cluster_NAMES$PROP_AGE)


data_post_cluster_NAMES$FLOOR_AREA_BAND <- ifelse(grepl("1", data_post_cluster_NAMES$FLOOR_AREA_BAND, ignore.case = TRUE), 
                                           "1 to 50 m2", data_post_cluster_NAMES$FLOOR_AREA_BAND)
data_post_cluster_NAMES$FLOOR_AREA_BAND <- ifelse(grepl("2", data_post_cluster_NAMES$FLOOR_AREA_BAND, ignore.case = TRUE), 
                                                  "51-100 m2", data_post_cluster_NAMES$FLOOR_AREA_BAND)
data_post_cluster_NAMES$FLOOR_AREA_BAND <- ifelse(grepl("3", data_post_cluster_NAMES$FLOOR_AREA_BAND, ignore.case = TRUE), 
                                                  "101-150 m2", data_post_cluster_NAMES$FLOOR_AREA_BAND)
data_post_cluster_NAMES$FLOOR_AREA_BAND <- ifelse(grepl("4", data_post_cluster_NAMES$FLOOR_AREA_BAND, ignore.case = TRUE), 
                                                  "Over 151 m2", data_post_cluster_NAMES$FLOOR_AREA_BAND)


data_post_cluster_NAMES$EE_BAND <- ifelse(grepl("1", data_post_cluster_NAMES$EE_BAND, ignore.case = TRUE), 
                                                  "Band A or B", data_post_cluster_NAMES$EE_BAND)
data_post_cluster_NAMES$EE_BAND <- ifelse(grepl("2", data_post_cluster_NAMES$EE_BAND, ignore.case = TRUE), 
                                          "Band C", data_post_cluster_NAMES$EE_BAND)
data_post_cluster_NAMES$EE_BAND <- ifelse(grepl("3", data_post_cluster_NAMES$EE_BAND, ignore.case = TRUE), 
                                          "Band D", data_post_cluster_NAMES$EE_BAND)
data_post_cluster_NAMES$EE_BAND <- ifelse(grepl("4", data_post_cluster_NAMES$EE_BAND, ignore.case = TRUE), 
                                          "Band E", data_post_cluster_NAMES$EE_BAND)
data_post_cluster_NAMES$EE_BAND <- ifelse(grepl("5", data_post_cluster_NAMES$EE_BAND, ignore.case = TRUE), 
                                          "Band F", data_post_cluster_NAMES$EE_BAND)
data_post_cluster_NAMES$EE_BAND <- ifelse(grepl("6", data_post_cluster_NAMES$EE_BAND, ignore.case = TRUE), 
                                          "Band G", data_post_cluster_NAMES$EE_BAND)


data_post_cluster_NAMES$LOFT_DEPTH <- ifelse(grepl("1", data_post_cluster_NAMES$LOFT_DEPTH, ignore.case = TRUE), 
                                          "Less than 150mm", data_post_cluster_NAMES$LOFT_DEPTH)
data_post_cluster_NAMES$LOFT_DEPTH <- ifelse(grepl("2", data_post_cluster_NAMES$LOFT_DEPTH, ignore.case = TRUE), 
                                             "Greater than or equal to 150mm", data_post_cluster_NAMES$LOFT_DEPTH)
data_post_cluster_NAMES$LOFT_DEPTH <- replace(data_post_cluster_NAMES$LOFT_DEPTH,
                              is.na(data_post_cluster_NAMES$LOFT_DEPTH), "no information") 


data_post_cluster_NAMES$WALL_CONS <- ifelse(grepl("1", data_post_cluster_NAMES$WALL_CONS, ignore.case = TRUE), 
                                             "Cavity wall", data_post_cluster_NAMES$WALL_CONS)
data_post_cluster_NAMES$WALL_CONS <- ifelse(grepl("2", data_post_cluster_NAMES$WALL_CONS, ignore.case = TRUE), 
                                            "Other", data_post_cluster_NAMES$WALL_CONS)


data_post_cluster_NAMES$CWI <- ifelse(grepl("1", data_post_cluster_NAMES$CWI, ignore.case = TRUE), 
                                            "CWI_government", data_post_cluster_NAMES$CWI)
data_post_cluster_NAMES$CWI <- replace(data_post_cluster_NAMES$CWI,
                                is.na(data_post_cluster_NAMES$CWI), "no cavity record") 


data_post_cluster_NAMES$BOILER <- ifelse(grepl("1", data_post_cluster_NAMES$BOILER, ignore.case = TRUE), 
                                      "New boiler installed", data_post_cluster_NAMES$BOILER)
data_post_cluster_NAMES$BOILER <- replace(data_post_cluster_NAMES$BOILER,
                             is.na(data_post_cluster_NAMES$BOILER), "no boiler record") 





data_post_cluster %>% 
  ggplot(aes(HH_ID, energy, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap(~cluster)

data_post_cluster %>% 
  ggplot(aes(HH_ID, Econs2012, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap(~cluster)

data_post_cluster %>% 
  ggplot(aes(HH_ID, Gcons2012, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap(~cluster)




summary_Consumption <- data_post_cluster %>% 
  group_by(cluster) %>% 
  summarise(energy = mean(energy, na.rm = TRUE), 
            electricity = mean(Econs2012, na.rm = TRUE),
            gas = mean(Gcons2012, na.rm = TRUE))


jpeg('C:/NEED/plots/Energy_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

summary_Consumption %>% 
  ggplot(aes(cluster, energy, fill = cluster)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
ggtitle("Total Energy") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()



summary_Consumption %>% 
  ggplot(aes(cluster, electricity, fill = cluster)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Summary Electricity (2012)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

summary_Consumption %>% 
  ggplot(aes(cluster, gas, fill = cluster)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Summary Gas (2012)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))





IMD <- data_post_cluster_NAMES %>%  
  group_by(cluster,inv_IMD_ENG) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/inv_IMD_ENG_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

IMD %>% 
  ggplot(aes(cluster, n, fill = inv_IMD_ENG))  +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) + geom_bar(stat = "identity") +
  theme(axis.text=element_text(size=14,face="bold", colour = "black")) +
  ggtitle("1/IMD (deprivation)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()




TYPE <- data_post_cluster %>%  
  group_by(cluster,PROP_TYPE) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/prop_type_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

TYPE %>% 
  ggplot(aes(cluster, n, fill = PROP_TYPE))  +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) + geom_bar(stat = "identity") +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Property Type") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))


par(oldpar)
dev.off()




AREA <- data_post_cluster_NAMES %>%  
  group_by(cluster,FLOOR_AREA_BAND) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/area_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

AREA %>% 
  ggplot(aes(cluster, n, fill = FLOOR_AREA_BAND))  +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) + geom_bar(stat = "identity") +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Floor Area Band") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()





EE <- data_post_cluster_NAMES %>%  
  group_by(cluster,EE_BAND) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/EE_Band_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

EE %>% 
  ggplot(aes(cluster, n, fill = EE_BAND))  +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) + geom_bar(stat = "identity") +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("EE Band") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()




#######################################################################

DEPTH <- data_post_cluster_NAMES %>%  
  group_by(cluster,LOFT_DEPTH) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/loft_depth_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

DEPTH %>% 
  ggplot(aes(cluster, n, fill = LOFT_DEPTH)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Loft Depth") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()




CAVITY <- data_post_cluster_NAMES %>%  
  group_by(cluster,WALL_CONS) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/cavity_wall_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

CAVITY %>% 
  ggplot(aes(cluster, n, fill = WALL_CONS)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Wall Construction") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()




CWI_gov <- data_post_cluster_NAMES %>%  
  group_by(cluster,CWI) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/goverment_CWI_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

CWI_gov %>% 
  ggplot(aes(cluster, n, fill = CWI)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Cavity Wall Insulated") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()


#AAA <- data_post_cluster %>% 
 # subset(BOILER==1 & BOILER_YEAR<2012) 


BOILER_records <- data_post_cluster_NAMES %>%  
  group_by(cluster,BOILER) %>% 
  summarise(n = n())

jpeg('C:/NEED/plots/boiler_5_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

BOILER_records %>% 
  ggplot(aes(cluster, n, fill = BOILER)) + geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Boiler") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14))

par(oldpar)
dev.off()




#######  Calculate quartiles within clusters #####################

TOT_ENERGY <- data_post_cluster_NAMES %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(energy, 4)) %>% 
  ungroup()


# Arrange by numeric values
DATA_FINAL_ALL <- TOT_ENERGY %>% 
  arrange(cluster, quartile, energy)

# write.csv(DATA_FINAL_ALL, file = "DATA_FINAL_ALL.csv", row.names=TRUE)

# Counts of flats per cluster quartile
TOT_ENERGY %>%  
  group_by(cluster, quartile) %>% 
  summarise(n = n())


# # Get HH_ID with high consumption for their cluster
# data_high <- TOT_ENERGY %>% 
#   filter(quartile == 4)

# View(a[, 50:101])

DATA_ENERGY <- TOT_ENERGY %>% 
  arrange(cluster, quartile, energy) 
 
# %>%
  #select(HH_ID, cluster,quartile, energy)

DATA_ENERGY$quartile[DATA_ENERGY$quartile==1] <- "LL"
DATA_ENERGY$quartile[DATA_ENERGY$quartile==2] <- "L"
DATA_ENERGY$quartile[DATA_ENERGY$quartile==3] <- "H"
DATA_ENERGY$quartile[DATA_ENERGY$quartile==4] <- "HH"

write.csv(DATA_ENERGY, file = "Ranking_and_Clustering_DATA_ENERGY.csv", row.names=TRUE)

Summary_Energy <- TOT_ENERGY %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_Energy$quartile[Summary_Energy$quartile==1] <- "LL"
Summary_Energy$quartile[Summary_Energy$quartile==2] <- "L"
Summary_Energy$quartile[Summary_Energy$quartile==3] <- "H"
Summary_Energy$quartile[Summary_Energy$quartile==4] <- "HH"
# 
# Summary_Energy %>% 
#   ggplot(aes(cluster, n, fill = cluster)) + geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
#   ggtitle("Summary n.properties") + 
#   theme(plot.title = element_text(lineheight=.8, face="bold"))
# 
# 
# Summary_Energy %>% 
#   ggplot(aes(cluster, n, fill = quartile)) + geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
#   ggtitle("Summary n. properties") + 
#   theme(plot.title = element_text(lineheight=.8, face="bold"))

# write.csv(Summary_Energy, file = "Summary_Energy.csv", row.names=TRUE)

##############################################################

#### Gas #####################################################

jpeg('C:/NEED/plots/Gas_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(Gcons2012)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Gas 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()


#data_energy_gas <- data_energy %>%
 # filter(!is.na(IMD_ENG)) %>%

BOILER_NULL <- data_post_cluster %>%
  subset(is.na(BOILER))

BOILER_OK <- data_post_cluster %>%
  subset(BOILER ==1)

BOILER_Before2012 <- data_post_cluster %>%
  subset(BOILER == 1 & BOILER_YEAR < 2012)  

BOILER_2012 <- data_post_cluster %>%
  subset(BOILER ==1 & BOILER_YEAR == 2012) 

Data_Cluster_1 <- data_post_cluster %>%
  subset(cluster == 1) 

Data_Cluster_2 <- data_post_cluster %>%
  subset(cluster == 2) 

Data_Cluster_3 <- data_post_cluster %>%
  subset(cluster == 3) 

Data_Cluster_4 <- data_post_cluster %>%
  subset(cluster == 4) 



jpeg('C:/NEED/plots/Gas_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(Gcons2012, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
   geom_density(alpha = 0.5) +
  ggtitle("Gas 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()


###### Electricity #########

jpeg('C:/NEED/plots/Electricity_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(Econs2012)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Electricity 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))
  

par(oldpar)
dev.off()


jpeg('C:/NEED/plots/Electriciy_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(Econs2012, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Electricity 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



####### Total Energy #####################################

jpeg('C:/NEED/plots/Energy_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(energy)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Total Energy 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Energy_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(energy, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Total Energy 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



####### Gas vs floor area ###########################################

jpeg('C:/NEED/plots/Gas_vs_area_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(gas_floor_area)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Gas/Floor Area Band 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Gas_vs_area_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(gas_floor_area, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Gas/Floor Area Band 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()


######### Electricity ve floor area ########

jpeg('C:/NEED/plots/Electricity_vs_area_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(electricity_floor_area)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Electricity/Floor Area Band 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Electricity_vs_area_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(electricity_floor_area, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Electricity/Floor Area Band 2012") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))
par(oldpar)
dev.off()


###### Total Energy vs 1/IMD #########

jpeg('C:/NEED/plots/Energy_vs_inv_IMD_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(energy_inv_IMD_ENG)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Energy/ (1/IMD)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Energy_vs_inv_IMD_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(energy_inv_IMD_ENG, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Energy/ (1/IMD)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()


data_post_cluster <- read.csv("data_post_cluster.csv")
data_post_cluster$energy_IMD_ENG <- (data_post_cluster$energy)/(data_post_cluster$IMD_ENG)
write.csv(data_post_cluster, file = "data_post_cluster.csv", row.names=TRUE)

###### Total Energy vs IMD #########

jpeg('C:/NEED/plots/Energy_vs_IMD_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(energy_IMD_ENG)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Energy / IMD") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Energy_vs_IMD_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(energy_IMD_ENG, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Energy / IMD") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()




###### Total Energy vs EE band #########

jpeg('C:/NEED/plots/Energy_vs_EE_band_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>%  ### without clustering
  ggplot(aes(energy_EE_BAND)) + 
  geom_density(fill = "dodgerblue", alpha = .5) +
  ggtitle("Energy/EE band") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()



jpeg('C:/NEED/plots/Energy_vs_EE_band_cluster_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_post_cluster %>% 
  ggplot(aes(energy_EE_BAND, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5) +
  ggtitle("Energy/EE band") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size=18, colour = "black"))

par(oldpar)
dev.off()


