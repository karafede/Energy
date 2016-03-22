# Set-up ---------------------------
# Load packages
# library(threadr) package builf by Stuart Grange (@ Ricardo Energy & Environment)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)

# Set global options
options(stringsAsFactors = FALSE)

# Clear all objects
rm(list = ls(all = TRUE))

data_energy <- read.csv("C:/Warwick_copy/university_of_warwick_energy_and_occupant_tidy_data_joined.csv") %>%  
mutate(date = ymd(date))
# mutate(data_energy,date = ymd(date))

# data_epc <- read.csv("university_of_warwick_epc_data.csv.bz2")
data_epc <- read.csv("C:/Warwick_copy/university_of_warwick_epc_tidy_data.csv")

# Join energy data to epc data
# Aggregate consumption data

summary_energy <- data_energy %>%
  group_by(flat_name, 
           flat_number) %>%
  summarise(electricity = mean(electricity), 
            gas = mean(gas), 
            occupant_adult = mean(occupant_adult), 
            occupant_child = mean(occupant_child), 
            occupant_sum = mean(occupant_sum)) %>%
  ungroup() %>% 
  arrange(flat_number)


# Join  (summary energy + data_epc)
data_epc <- data_epc %>% 
  inner_join(summary_energy, c("flat_name", "flat_number")) ## "flat name and flat number are the common fields to join

data_epc %>% 
  ggplot(aes(flat_name, energy_rating_current)) + 
  geom_point(colour = "darkred", size = 4) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))

data_epc %>% 
  ggplot(aes(energy_rating_current)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

data_epc %>% 
  ggplot(aes(log(energy_rating_current))) + 
  geom_density(fill = "dodgerblue", alpha = .5)

data_epc %>% 
  ggplot(aes(flat_name, environment_impact_current)) + 
  geom_point(colour = "darkgreen", size = 4) +
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))


data_epc %>% 
  ggplot(aes(occupant_sum, electricity)) + geom_point(colour = "red", size = 4) + 
  stat_smooth()

data_epc %>% 
  ggplot(aes(occupant_sum, gas)) + geom_point(colour = "orange", size = 4) + 
  stat_smooth()

############################################################################

data_energy %>% 
  group_by(occupant_sum) %>% 
  summarise(electricity = mean(electricity),
            gas = mean(gas)) %>%
  gather(key, value, -occupant_sum) %>%
  ggplot(aes(occupant_sum, value, colour = as.factor(occupant_sum), group = 1)) + 
  geom_point(size = 5) + facet_wrap("key", scales = "free_y") + 
  stat_smooth(method = "lm")

############### Additional Eleaborations  #####################################

#### assign numbers to factor names #######
data_epc$Heat_loss <- as.numeric(as.factor(data_epc$floor_heat_loss))
data_epc$Roof_description <- as.numeric(as.factor(data_epc$roof_description))
data_epc$Energy <- data_epc$electricity + data_epc$gas


data_epc %>% 
  ggplot(aes(occupant_sum, electricity)) + 
  geom_point(colour = "darkred", size = 4) + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))


data_epc %>% 
  group_by(Heat_loss) %>% 
  summarise(electricity = mean(electricity),
            gas = mean(gas)) %>%
  gather(key, value, -Heat_loss) %>%
  ggplot(aes(Heat_loss, value, colour = as.factor(Heat_loss), group = 1)) + 
  geom_point(size = 5) + facet_wrap("key", scales = "free_y") + 
  stat_smooth(method = "lm")


# data_epc$energy_occupants <- data_epc$electricity / data_epc$occupant_sum
# data_epc$energy_child <- data_epc$electricity / data_epc$occupant_child
# data_epc$energy_child [!is.finite(data_epc$energy_child )] <- 0
# data_epc$energy_adult <- data_epc$electricity / data_epc$occupant_adult
# data_epc$energy_floor_area <- data_epc$electricity / data_epc$total_floor_area
# data_epc$energy_bedrooms <- data_epc$electricity / data_epc$bedrooms
 #data_epc$energy_current_rating <- data_epc$electricity / data_epc$energy_rating_current
# data_epc$energy_heat_loss <- data_epc$electricity / data_epc$Heat_loss
# data_epc$energy_roof <- data_epc$electricity / data_epc$Roof_description
# data_epc$energy_roof <- data_epc$electricity / data_epc$roof_energy_level
 #data_epc$energy_walls <- data_epc$electricity / data_epc$walls_energy_level



data_epc$energy_occupants <- data_epc$Energy / data_epc$occupant_sum
data_epc$energy_child <- data_epc$Energy / data_epc$occupant_child
data_epc$energy_child [!is.finite(data_epc$energy_child )] <- 0
data_epc$energy_adult <- data_epc$Energy / data_epc$occupant_adult
data_epc$energy_floor_area <- data_epc$Energy / data_epc$total_floor_area
data_epc$energy_bedrooms <- data_epc$Energy / data_epc$bedrooms
data_epc$energy_current_rating <- data_epc$Energy / data_epc$energy_rating_current
data_epc$energy_heat_loss <- data_epc$Energy / data_epc$Heat_loss
data_epc$energy_roof <- data_epc$Energy / data_epc$Roof_description
#data_epc$energy_roof <- data_epc$Energy / data_epc$roof_energy_level
data_epc$energy_walls <- data_epc$Energy / data_epc$walls_energy_level

data_epc_select <- data_epc %>% 
  select(flat_name, 
   bedrooms,
   total_floor_area,
   floor_level_level,
   electricity,
   gas,
  #occupant_sum,
   occupant_adult,
   occupant_child) %>% 
  na.omit()


data_epc_select <- data_epc %>% 
  select(flat_name, 
         energy_occupants,
         #energy_child,
         #energy_adult,
         energy_floor_area,
         energy_bedrooms,
         energy_current_rating,
         energy_roof,
         energy_heat_loss) %>% 
  na.omit()

# Store unique identifiers
rownames <- data_epc_select$flat_name

# Give data frame unique row names
row.names(data_epc_select) <- rownames
data_epc_select$flat_name <- NULL

# Standardise variables (rescale data based on the meand and Standard deviation)
# data_epc_select_standard <- standardise(data_epc_select)
data_epc_select_standard <- data.frame(scale(data_epc_select))

## Plot the pairwise scatterplot
# pairs(data_epc_select)

############### PRINCIPAL COMPONENT ANALYSIS ################

EPC.pca <- prcomp(data_epc_select_standard,
                 center = TRUE,
                 scale. = TRUE)

plot(EPC.pca, type = "l")
summary(EPC.pca) ### cumulative
plot(EPC.pca)
# biplot(EPC.pca)

################ CLUSTER ANALYSIS #############################

# Calculate distance matrix
distance <- dist(data_epc_select_standard, method = "euclidean")
see_distance <- as.matrix(distance)

# Do a Ward hierarchical cluster analyis
fit <- hclust(distance, method = "ward.D")

# Dendogram
plot(fit, labels = FALSE)

# Create a cluster vector
# Clusters
k <- 3

# Group ## cut the cluster
cluster <- cutree(fit, k = k)
see_cluster <- as.matrix(cluster)

# Plot
rect.hclust(fit, k = k, border = "red")
# graphics.off()

# Give observations cluster variable 
data_epc_post_cluster <- data_epc_select %>% 
  mutate(flat_name = rownames,         ### add flat name (new column)
         cluster = unname(cluster)) %>%   ### add cluster column
  arrange(cluster)

# Join cluster group to data
# Select
data_epc_post_cluster <- data_epc_post_cluster %>% 
  select(flat_name, cluster)


data_epc_post_cluster <- data_epc %>% 
  inner_join(data_epc_post_cluster, "flat_name") ## "flat_name" is the common field to join

write.csv(data_epc_post_cluster, file = "NORMALIZED_data_epc_post_cluster.csv", row.names=TRUE)

##### Subset Clusters ########################################################

cluster_1 <- subset(data_epc_post_cluster, cluster == 1)
cluster_2 <- subset(data_epc_post_cluster, cluster == 2)
cluster_3 <- subset(data_epc_post_cluster, cluster == 3)

#################################################################################


####### Plots #################################################################
###### cluster characteization ###############################################


jpeg('C:/Warwick_copy/plots/flats_vs_floorLevel.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(flat_name, floor_level_level, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()



jpeg('C:/Warwick_copy/plots/flats_vs_RoofType.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(flat_name, Roof_description, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()


jpeg('C:/Warwick_copy/plots/flat&child_points_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(flat_name, occupant_child, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()



data_epc_post_cluster %>% 
  ggplot(aes(flat_name, occupant_adult, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")


data_epc_post_cluster %>% 
  ggplot(aes(flat_name, flat_name, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")



jpeg('C:/Warwick_copy/plots/flat&bedrooms_points_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(flat_name, bedrooms, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()


jpeg('C:/Warwick_copy/plots/adult&bedrooms_points_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(occupant_adult, bedrooms, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()



jpeg('C:/Warwick_copy/plots/child&bedrooms_points_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(occupant_child, bedrooms, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()

####### Distributions for normalized variables ###############################


jpeg('C:/Warwick_copy/plots/Energy_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(Energy, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()

ENE_upperquart <- cluster_1[cluster_1$Energy > quantile(cluster_1$Energy , 0.75 ) , ]
ENE_upperquart <- cluster_2[cluster_2$Energy > quantile(cluster_2$Energy , 0.75 ) , ]
ENE_upperquart <- cluster_3[cluster_3$Energy > quantile(cluster_3$Energy , 0.75 ) , ]




jpeg('C:/Warwick_copy/plots/energy_vs_occupants_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_occupants, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()

##### Subset flats above the 75% of the top electricity/occupants values #####

ENE_vs_occup_upperquart <- cluster_1[cluster_1$energy_occupants > quantile(cluster_1$energy_occupants , 0.75 ) , ]

ENE_vs_occup_upperquart <- cluster_2[cluster_2$energy_occupants > quantile(cluster_2$energy_occupants , 0.75 ) , ]

ENE_vs_occup_upperquart <- cluster_3[cluster_3$energy_occupants > quantile(cluster_3$energy_occupants , 0.75 ) , ]

################################################################################


jpeg('C:/Warwick_copy/plots/energy_vs_occupants_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_occupants)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()




jpeg('C:/Warwick_copy/plots/energy_vs_floorarea_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_floor_area, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()


##### Subset flats above the 75% of the top electricity/floor area values #####

ENE_vs_total_floor_area <- cluster_1[cluster_1$energy_floor_area > quantile(cluster_1$energy_floor_area , 0.75 ) , ]

ENE_vs_total_floor_area <- cluster_2[cluster_2$energy_floor_area > quantile(cluster_2$energy_floor_area , 0.75 ) , ]

ENE_vs_total_floor_area <- cluster_3[cluster_3$energy_floor_area > quantile(cluster_3$energy_floor_area , 0.75 ) , ]


################################################################################



jpeg('C:/Warwick_copy/plots/energy_vs_floorarea_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_floor_area)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()






jpeg('C:/Warwick_copy/plots/energy_vs_bedrooms_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_bedrooms, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()

##### Subset flats above the 75% of the top electricity/bedrooms area values #####

ENE_vs_bedrooms <- cluster_1[cluster_1$energy_bedrooms > quantile(cluster_1$energy_bedrooms , 0.75 ) , ]

ENE_vs_bedrooms <- cluster_2[cluster_2$energy_bedrooms > quantile(cluster_2$energy_bedrooms , 0.75 ) , ]

ENE_vs_bedrooms <- cluster_3[cluster_3$energy_bedrooms > quantile(cluster_3$energy_bedrooms , 0.75 ) , ]

#############################################################################

jpeg('C:/Warwick_copy/plots/energy_vs_bedrooms_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_bedrooms)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()






jpeg('C:/Warwick_copy/plots/energy_vs_energy_rating_current_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_current_rating, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()

##### Subset flats above the 75% of the top electricity/current rating values #####

ENE_vs_current <- cluster_1[cluster_1$energy_current_rating > quantile(cluster_1$energy_current_rating , 0.75 ) , ]

ENE_vs_current <- cluster_2[cluster_2$energy_current_rating > quantile(cluster_2$energy_current_rating , 0.75 ) , ]

ENE_vs_current <- cluster_3[cluster_3$energy_current_rating > quantile(cluster_3$energy_current_rating , 0.75 ) , ]

#############################################################################



jpeg('C:/Warwick_copy/plots/energy_vs_energy_rating_current_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_current_rating)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()





jpeg('C:/Warwick_copy/plots/energy_vs_heat_loss_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_heat_loss, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()


##### Subset flats above the 75% of the top electricity/heat loss values #####

ENE_vs_heat_loss <- cluster_1[cluster_1$energy_heat_loss > quantile(cluster_1$energy_heat_loss , 0.75 ) , ]

ENE_vs_heat_loss <- cluster_2[cluster_2$energy_heat_loss > quantile(cluster_2$energy_heat_loss , 0.75 ) , ]

ENE_vs_heat_loss <- cluster_3[cluster_3$energy_heat_loss > quantile(cluster_3$energy_heat_loss , 0.75 ) , ]

#############################################################################



jpeg('C:/Warwick_copy/plots/energy_vs_heat_loss_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_heat_loss)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()







jpeg('C:/Warwick_copy/plots/energy_vs_roofType_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_roof, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()

##### Subset flats above the 75% of the top electricity/roof type values #####

ENE_vs_roof_type <- cluster_1[cluster_1$energy_roof > quantile(cluster_1$energy_roof , 0.75 ) , ]

ENE_vs_roof_type <- cluster_2[cluster_2$energy_roof > quantile(cluster_2$energy_roof , 0.75 ) , ]

ENE_vs_roof_type <- cluster_3[cluster_3$energy_roof > quantile(cluster_3$energy_roof , 0.75 ) , ]

#############################################################################



jpeg('C:/Warwick_copy/plots/energy_vs_energy_roofType_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_roof)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()

