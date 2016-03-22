# Set-up ---------------------------
# Load packages
# library(threadr) package builf by Stuart Grange (@ Ricardo Energy & Environment)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(gtools)
library(devtools)
library(rCharts)

# Set global options
options(stringsAsFactors = FALSE)

# Clear all objects
rm(list = ls(all = TRUE))

setwd("C:/Warwick_copy")
data_energy <- read.csv("C:/Warwick_copy/university_of_warwick_energy_and_occupant_tidy_data_joined.csv") %>%  
mutate(date = ymd(date))

# data_epc <- read.csv("university_of_warwick_epc_data.csv.bz2")
data_epc <- read.csv("C:/Warwick_copy/university_of_warwick_epc_tidy_data.csv")

# data_energy %>% 
#   ggplot(aes(data_energy$date, data_energy$electricity)) + 
#    geom_point(colour = "red", size = 2) + 
#   geom_smooth(method = "loess", se=FALSE, color="black", size = 1.5, aes(group=1))


data_energy$gas <- (data_energy$gas)*10.972

# data_energy %>% 
#   ggplot(aes(data_energy$date, data_energy$gas)) + 
#   geom_point(colour = "red", size = 2) + 
#    geom_smooth(method = "loess", se=FALSE, color="black", size = 1.5, aes(group=1))

data_energy$TOT_ENERGY <- data_energy$electricity + data_energy$gas

# data_energy %>% 
  # ggplot(aes(data_energy$date, data_energy$TOT_ENERGY)) + 
  # geom_point(colour = "red", size = 2) + 
  # geom_smooth(method = "loess", se=FALSE, color="black", size = 1.5, aes(group=1))

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
  inner_join(summary_energy, c("flat_name", "flat_number")) ## "flat name and numver are the common fields to join

############################################################################

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
  ggplot(aes(environment_impact_current)) + 
  geom_density(fill = "green", alpha = .5)

data_epc %>% 
  ggplot(aes(log(environment_impact_current))) + 
  geom_density(fill = "green", alpha = .5)


data_epc %>% 
  ggplot(aes(total_floor_area)) + 
  geom_density(fill = "green", alpha = .5)

data_epc %>% 
  ggplot(aes(electricity)) + 
  geom_density(fill = "green", alpha = .5)


data_epc %>% 
  ggplot(aes(occupant_sum, electricity)) + geom_point(colour = "red", size = 4) + 
  stat_smooth()

data_epc %>% 
  ggplot(aes(occupant_sum, gas)) + geom_point(colour = "orange", size = 4) + 
  stat_smooth()

############################################################################

library(gclus)

data_energy %>% 
  group_by(occupant_sum) %>% 
  summarise(electricity = mean(electricity),
            gas = mean(gas)) %>%
  gather(key, value, -occupant_sum) %>%
  ggplot(aes(occupant_sum, value, colour = as.factor(occupant_sum), group = 1)) + 
  geom_point(size = 5) + facet_wrap("key", scales = "free_y") + 
  stat_smooth(method = "lm")

summary_energy %>% 
  ggplot(aes(occupant_sum, electricity, group = 1)) + 
  geom_point(size = 5) + 
  stat_smooth(method = "lm") +
 theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size=12,face="bold", colour = "black")) +
  ggtitle("Electricity vs occupants") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=18))


# Linear Regression

plot(x=summary_energy$occupant_sum,  y=summary_energy$electricity, xlab="occupant_sum",
     ylab="electricity",main="electricity vs occupant_sum")
fit <- lm(electricity ~ occupant_sum, data = summary_energy)
abline(fit, col = "red")
summary(fit) # show results
summary(fit)$coefficients[,4] 
summary(fit)$r.squared


# Add fit lines
abline(lm(electricity ~ occupant_sum, data = summary_energy), col="red") # regression line (y~x) 
lines(lowess(summary_energy$occupant_sum, summary_energy$electricity), col="blue") # lowess line (x,y)



regrline = function(x,y) {
  points(x,y,pch=".")
  # abline(line(x,y),col="blue")  ### add a smooth line
  abline(lsfit(x,y),col="red")   ### add a regression line
}


panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(1, 0, 1, 0))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  #Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.5,cex = 2,  paste("r =",txt))
  #text(.5, .75, Signif)
}


panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}



summary <- select(summary_energy,
                  - flat_name)

# Basic Scatterplot Matrix
pairs(summary, 
      lower.panel = regrline,
      # lower.panel = panel.smooth,
      upper.panel = panel.cor,
      diag.panel =  panel.hist,
      main="Simple Scatterplot Matrix")


summary.r <- abs(cor(summary)) # get correlations
summary.col <- dmat.color(summary.r) # get colors
# reorder variables so those with highest correlation are closest to the diagonal
summary.o <- order.single(summary.r) 

cpairs(summary, summary.o, panel.colors=summary.col, gap=.5,
       lower.panel = regrline, 
       diag.panel =  panel.hist,
       main="Variables Ordered and Colored by Correlation" )


# Multiple Linear Regression Example 
fit <- lm(electricity ~ occupant_adult, data=summary)
fit <- lm(electricity ~ occupant_adult + occupant_child, data=summary)

summary(fit) # show results

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

summary(fit)$coefficients[,4] 
summary(fit)$r.squared



###############################################################################


data_energy %>% 
  group_by(occupant_adult) %>% 
  summarise(electricity = mean(electricity),
            gas = mean(gas)) %>%
  gather(key, value, -occupant_adult) %>%
  ggplot(aes(occupant_adult, value, colour = as.factor(occupant_adult), group = 1)) + 
  geom_point(size = 5) + facet_wrap("key", scales = "free_y") + 
  stat_smooth(method = "lm")

data_energy %>% 
  group_by(occupant_child) %>% 
  summarise(electricity = mean(electricity),
            gas = mean(gas)) %>%
  gather(key, value, -occupant_child) %>%
  ggplot(aes(occupant_child, value, colour = as.factor(occupant_child), group = 1)) + 
  geom_point(size = 5) + facet_wrap("key", scales = "free_y") + 
  stat_smooth(method = "lm")



############### Additional Eleaborations  #####################################


#### assign numbers to factor names #######
data_epc$Heat_loss <- as.numeric(as.factor(data_epc$floor_heat_loss))
data_epc$Roof_description <- as.numeric(as.factor(data_epc$roof_description))
data_epc$gas <- 10.972*data_epc$gas
data_epc$energy_consumption_current <- (data_epc$total_floor_area)*(data_epc$energy_consumption_current)
data_epc$Energy <- data_epc$electricity + data_epc$gas
data_epc$Energy_RATING <- 1/(data_epc$energy_rating_current)

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
# data_epc$energy_current_rating <- data_epc$electricity / data_epc$energy_rating_current
# data_epc$energy_heat_loss <- data_epc$electricity / data_epc$Heat_loss
# data_epc$energy_roof <- data_epc$electricity / data_epc$Roof_description
# data_epc$energy_roof <- data_epc$electricity / data_epc$roof_energy_level
# data_epc$energy_walls <- data_epc$electricity / data_epc$walls_energy_level

data_epc$energy_occupants <- data_epc$Energy / data_epc$occupant_sum
data_epc$energy_child <- data_epc$Energy / data_epc$occupant_child
data_epc$energy_child [!is.finite(data_epc$energy_child )] <- 0
data_epc$energy_adult <- data_epc$Energy / data_epc$occupant_adult
data_epc$energy_floor_area <- data_epc$Energy / data_epc$total_floor_area
data_epc$energy_bedrooms <- data_epc$Energy / data_epc$bedrooms
# data_epc$energy_current_rating <- data_epc$Energy / data_epc$energy_rating_current
data_epc$energy_predicted <- 365*(data_epc$Energy/data_epc$energy_consumption_current)
data_epc$energy_heat_loss <- data_epc$Energy / data_epc$Heat_loss
data_epc$energy_roof <- data_epc$Energy / data_epc$Roof_description
#data_epc$energy_roof <- data_epc$Energy / data_epc$roof_energy_level
data_epc$energy_walls <- data_epc$Energy / data_epc$walls_energy_level

# Select a few variables
  # data_epc_select <- data_epc %>% 
  # select(flat_name, 
    #     bedrooms,
     #    energy_rating_current,
      #   environment_impact_current,
       #  energy_consumption_current, 
        # co2_emissions_current, 
         # total_floor_area, 
         # mainheat_energy_level,
         # windows_energy_level,
         # hot_water_energy_level,
         # floor_level_level) %>% 
  # na.omit()

# roof_energy_level
# walls_energy_level,


data_epc_select <- data_epc %>% 
  select(flat_name, 
   bedrooms,
   total_floor_area, 
   floor_level_level,
  # Energy,
   #electricity,
   #gas,
  #occupant_sum,
   occupant_adult,
   occupant_child) %>% 
  na.omit()


# data_epc %>% 
 # ggplot(aes(log(energy_occupants))) + geom_density()


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

print(EPC.pca) ### Loadings
rotation_PCA <- EPC.pca$rotation
write.csv(rotation_PCA, file = "rotation_PCA.csv", row.names=TRUE)
aload <- abs(EPC.pca$rotation)
relative_PCA <- (sweep(aload, 2, colSums(aload), "/"))*100

write.csv(relative_PCA, file = "relative_PCA.csv", row.names=TRUE)

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
         cluster = unname(cluster)) %>%   ### add cluste column
  arrange(cluster)

# Join cluster group to data
# Select
data_epc_post_cluster <- data_epc_post_cluster %>% 
  select(flat_name, cluster)

# Join
# data_epc_post_cluster <- data_epc %>% 
  # inner_join(data_epc_post_cluster, "flat_name") ## "flat_name" is the common field to join

data_epc_post_cluster <- data_epc %>% 
  inner_join(data_epc_post_cluster, "flat_name") ## "flat_name" is the common field to join

write.csv(data_epc_post_cluster, file = "data_epc_post_cluster.csv", row.names=TRUE)


data_epc_post_cluster %>% 
  ggplot(aes(flat_name, flat_name, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

cluster_1 <- subset(data_epc_post_cluster, cluster == 1)
cluster_2 <- subset(data_epc_post_cluster, cluster == 2)
cluster_3 <- subset(data_epc_post_cluster, cluster == 3)

####### Plots ###########################################

# jpeg('C:/Warwick_copy/electricity_clusters_points.jpg',
  #   quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(flat_name, Energy, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap(~cluster)

# par(oldpar)
# dev.off()


###############

jpeg('C:/Warwick_copy/plots/Energy_vs_floorLevel.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(floor_level_level, Energy, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()



jpeg('C:/Warwick_copy/plots/Energy_vs_RoofType.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(Roof_description, Energy, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()


################# Total Energy  ####################################################


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




jpeg('C:/Warwick_copy/plots/Energy_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(Energy)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()


##### Subset flats above the 75% of the top electricity values ###############

cluster_1 <- subset(data_epc_post_cluster, cluster == 1)
cluster_2 <- subset(data_epc_post_cluster, cluster == 2)
cluster_3 <- subset(data_epc_post_cluster, cluster == 3)

# ENE_firstquart <- cluster_1[cluster_1$Energy < quantile(cluster_1$Energy , 0.25), ]
# ENE_firstquart <- cluster_2[cluster_2$Energy < quantile(cluster_2$Energy , 0.25 ) , ]
# ENE_firstquart <- cluster_3[cluster_3$Energy < quantile(cluster_3$Energy , 0.25 ) , ]


# ENE_secondquart <- cluster_1[
  # ifelse(cluster_1$Energy > quantile(cluster_1$Energy, 0.25) & 
    #        cluster_1$Energy < quantile(cluster_1$Energy, 0.5), TRUE, FALSE), ]

# ENE_thirdquart <- cluster_1[
  # ifelse(cluster_1$Energy > quantile(cluster_1$Energy, 0.5) & 
    #        cluster_1$Energy < quantile(cluster_1$Energy, 0.75), TRUE, FALSE), ]

# ENE_secondquart <- cluster_1[cluster_1$Energy > quantile(cluster_1$Energy , 0.25 ) , ]
# ENE_secondquart <- cluster_2[cluster_2$Energy > quantile(cluster_2$Energy , 0.25 ) , ]
# ENE_secondquart <- cluster_3[cluster_3$Energy > quantile(cluster_3$Energy , 0.25 ) , ]

# ENE_upperquart <- cluster_1[cluster_1$Energy > quantile(cluster_1$Energy , 0.75 ) , ]
# ENE_upperquart <- cluster_2[cluster_2$Energy > quantile(cluster_2$Energy , 0.75 ) , ]
# ENE_upperquart <- cluster_3[cluster_3$Energy > quantile(cluster_3$Energy , 0.75 ) , ]

# Calculate quartiles within clusters
TOT_ENERGY <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(Energy, 4)) %>% 
  ungroup()


# Arrange by numeric values
DATA_FINAL_ALL <- TOT_ENERGY %>% 
  arrange(cluster, quartile, Energy)


# Arrange by numeric values
DATA_FINAL_selected <- TOT_ENERGY %>% 
  arrange(cluster, quartile, Energy) %>% 
  select(flat_name, cluster,quartile, Energy, electricity, gas,
         energy_occupants, energy_floor_area,energy_bedrooms,
         Energy_RATING, energy_predicted, energy_heat_loss, energy_roof)

write.csv(DATA_FINAL_ALL, file = "DATA_FINAL_ALL.csv", row.names=TRUE)
write.csv(DATA_FINAL_selected, file = "DATA_FINAL_selected.csv", row.names=TRUE)


# Counts of flats per cluster quartile
TOT_ENERGY %>%  
  group_by(cluster, quartile) %>% 
  summarise(n = n())


# Get flats with high consumption for their cluster
data_high <- TOT_ENERGY %>% 
  filter(quartile == 4)

# View(a[, 50:101])



DATA_ENERGY <- TOT_ENERGY %>% 
  arrange(cluster, quartile, Energy) %>% 
  select(flat_name, cluster,quartile, Energy)

DATA_ENERGY$quartile[DATA_ENERGY$quartile==1] <- "LL"
DATA_ENERGY$quartile[DATA_ENERGY$quartile==2] <- "L"
DATA_ENERGY$quartile[DATA_ENERGY$quartile==3] <- "H"
DATA_ENERGY$quartile[DATA_ENERGY$quartile==4] <- "HH"

write.csv(DATA_ENERGY, file = "DATA_ENERGY.csv", row.names=TRUE)

# prova_1 <- DATA_ENERGY %>%
# filter(DATA_ENERGY$cluster ==1 & DATA_ENERGY$quartile ==1)
# prova_2 <- DATA_ENERGY %>%
#   filter(DATA_ENERGY$cluster ==1 & DATA_ENERGY$quartile ==2)
# prova <- combine(list(prova_1, prova_2))

Summary_Energy <- TOT_ENERGY %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_Energy$quartile[Summary_Energy$quartile==1] <- "LL"
Summary_Energy$quartile[Summary_Energy$quartile==2] <- "L"
Summary_Energy$quartile[Summary_Energy$quartile==3] <- "H"
Summary_Energy$quartile[Summary_Energy$quartile==4] <- "HH"

write.csv(Summary_Energy, file = "Summary_Energy.csv", row.names=TRUE)



#####################################################################################

jpeg('C:/Warwick_copy/plots/flat&child_points_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(flat_name, occupant_child, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

par(oldpar)
dev.off()




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


####################### Electricity ##########################################################


jpeg('C:/Warwick_copy/plots/electricity_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(electricity, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()




jpeg('C:/Warwick_copy/plots/electricity_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(electricity)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()



#################################################################################

ELE <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(electricity, 4)) %>% 
  ungroup()


DATA_ELE <- ELE %>% 
  arrange(cluster, quartile, electricity) %>% 
  select(flat_name, cluster,quartile, electricity)

DATA_ELE$quartile[DATA_ELE$quartile==1] <- "LL"
DATA_ELE$quartile[DATA_ELE$quartile==2] <- "L"
DATA_ELE$quartile[DATA_ELE$quartile==3] <- "H"
DATA_ELE$quartile[DATA_ELE$quartile==4] <- "HH"

write.csv(DATA_ELE, file = "DATA_ELE.csv", row.names=TRUE)

Summary_electricity <- ELE %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_electricity$quartile[Summary_electricity$quartile==1] <- "LL"
Summary_electricity$quartile[Summary_electricity$quartile==2] <- "L"
Summary_electricity$quartile[Summary_electricity$quartile==3] <- "H"
Summary_electricity$quartile[Summary_electricity$quartile==4] <- "HH"

write.csv(Summary_electricity, file = "Summary_electricity.csv", row.names=TRUE)


############ GAS ##############################################################


jpeg('C:/Warwick_copy/plots/gas_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(gas, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()




jpeg('C:/Warwick_copy/plots/gas_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(gas)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()



#################################################################################

GAZ <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(gas, 4)) %>% 
  ungroup()


DATA_GAS <- GAZ %>% 
  arrange(cluster, quartile, gas) %>% 
  select(flat_name, cluster,quartile, gas)

DATA_GAS$quartile[DATA_GAS$quartile==1] <- "LL"
DATA_GAS$quartile[DATA_GAS$quartile==2] <- "L"
DATA_GAS$quartile[DATA_GAS$quartile==3] <- "H"
DATA_GAS$quartile[DATA_GAS$quartile==4] <- "HH"


write.csv(DATA_GAS, file = "DATA_GAS.csv", row.names=TRUE)

Summary_gas <- GAZ %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_gas$quartile[Summary_gas$quartile==1] <- "LL"
Summary_gas$quartile[Summary_gas$quartile==2] <- "L"
Summary_gas$quartile[Summary_gas$quartile==3] <- "H"
Summary_gas$quartile[Summary_gas$quartile==4] <- "HH"


write.csv(Summary_gas, file = "Summary_gas.csv", row.names=TRUE)


############## Total Energy/Occupants ############################################


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



jpeg('C:/Warwick_copy/plots/energy_vs_occupants_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_occupants)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()


#################################################################################

ENE_occupants <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(energy_occupants, 4)) %>% 
  ungroup()


DATA_ENE_occupants <- ENE_occupants %>% 
  arrange(cluster, quartile, energy_occupants) %>% 
  select(flat_name, cluster,quartile, energy_occupants)

DATA_ENE_occupants$quartile[DATA_ENE_occupants$quartile==1] <- "LL"
DATA_ENE_occupants$quartile[DATA_ENE_occupants$quartile==2] <- "L"
DATA_ENE_occupants$quartile[DATA_ENE_occupants$quartile==3] <- "H"
DATA_ENE_occupants$quartile[DATA_ENE_occupants$quartile==4] <- "HH"

write.csv(DATA_ENE_occupants, file = "DATA_ENE_occupants.csv", row.names=TRUE)

Summary_ENE_occupants <- ENE_occupants %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_occupants$quartile[Summary_ENE_occupants$quartile==1] <- "LL"
Summary_ENE_occupants$quartile[Summary_ENE_occupants$quartile==2] <- "L"
Summary_ENE_occupants$quartile[Summary_ENE_occupants$quartile==3] <- "H"
Summary_ENE_occupants$quartile[Summary_ENE_occupants$quartile==4] <- "HH"

write.csv(Summary_ENE_occupants, file = "Summary_ENE_occupants.csv", row.names=TRUE)



################ Total Energy/Total floor area ################################

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



jpeg('C:/Warwick_copy/plots/energy_vs_floorarea_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_floor_area)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()


#################################################################################

ENE_floorarea <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(energy_floor_area, 4)) %>% 
  ungroup()


DATA_ENE_floorarea <- ENE_floorarea %>% 
  arrange(cluster, quartile, energy_floor_area) %>% 
  select(flat_name, cluster,quartile, energy_floor_area)

DATA_ENE_floorarea$quartile[DATA_ENE_floorarea$quartile==1] <- "LL"
DATA_ENE_floorarea$quartile[DATA_ENE_floorarea$quartile==2] <- "L"
DATA_ENE_floorarea$quartile[DATA_ENE_floorarea$quartile==3] <- "H"
DATA_ENE_floorarea$quartile[DATA_ENE_floorarea$quartile==4] <- "HH"

write.csv(DATA_ENE_floorarea, file = "DATA_ENE_floorarea.csv", row.names=TRUE)


Summary_ENE_floorarea <- ENE_floorarea %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_floorarea$quartile[Summary_ENE_floorarea$quartile==1] <- "LL"
Summary_ENE_floorarea$quartile[Summary_ENE_floorarea$quartile==2] <- "L"
Summary_ENE_floorarea$quartile[Summary_ENE_floorarea$quartile==3] <- "H"
Summary_ENE_floorarea$quartile[Summary_ENE_floorarea$quartile==4] <- "HH"

write.csv(Summary_ENE_floorarea, file = "Summary_ENE_floorarea.csv", row.names=TRUE)


###################### Total Energy/n. bedrooms ###############################


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



jpeg('C:/Warwick_copy/plots/energy_vs_bedrooms_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_bedrooms)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()




#################################################################################

ENE_bedrooms <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(energy_bedrooms, 4)) %>% 
  ungroup()

View(ENE_bedrooms[, 50:103])

DATA_ENE_bedrooms <- ENE_bedrooms %>% 
  arrange(cluster, quartile, energy_bedrooms) %>% 
  select(flat_name, cluster,quartile, energy_bedrooms)

DATA_ENE_bedrooms$quartile[DATA_ENE_bedrooms$quartile==1] <- "LL"
DATA_ENE_bedrooms$quartile[DATA_ENE_bedrooms$quartile==2] <- "L"
DATA_ENE_bedrooms$quartile[DATA_ENE_bedrooms$quartile==3] <- "H"
DATA_ENE_bedrooms$quartile[DATA_ENE_bedrooms$quartile==4] <- "HH"

write.csv(DATA_ENE_bedrooms, file = "DATA_ENE_bedrooms.csv", row.names=TRUE)


Summary_ENE_bedrooms <- ENE_bedrooms %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_bedrooms$quartile[Summary_ENE_bedrooms$quartile==1] <- "LL"
Summary_ENE_bedrooms$quartile[Summary_ENE_bedrooms$quartile==2] <- "L"
Summary_ENE_bedrooms$quartile[Summary_ENE_bedrooms$quartile==3] <- "H"
Summary_ENE_bedrooms$quartile[Summary_ENE_bedrooms$quartile==4] <- "HH"

write.csv(Summary_ENE_bedrooms, file = "Summary_ENE_bedrooms.csv", row.names=TRUE)


###################### Energy rating ####################################


jpeg('C:/Warwick_copy/plots/energy_rating_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(Energy_RATING, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()



jpeg('C:/Warwick_copy/plots/energy_rating_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(Energy_RATING)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()



#################################################################################

ENE_rating <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(Energy_RATING, 4)) %>% 
  ungroup()


DATA_ENE_rating <- ENE_rating %>% 
  arrange(cluster, quartile, Energy_RATING) %>% 
  select(flat_name, cluster,quartile, Energy_RATING)

DATA_ENE_rating$quartile[DATA_ENE_rating$quartile==1] <- "LL"
DATA_ENE_rating$quartile[DATA_ENE_rating$quartile==2] <- "L"
DATA_ENE_rating$quartile[DATA_ENE_rating$quartile==3] <- "H"
DATA_ENE_rating$quartile[DATA_ENE_rating$quartile==4] <- "HH"

write.csv(DATA_ENE_rating, file = "DATA_ENE_rating.csv", row.names=TRUE)


Summary_ENE_rating <- ENE_rating %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_rating$quartile[Summary_ENE_rating$quartile==1] <- "LL"
Summary_ENE_rating$quartile[Summary_ENE_rating$quartile==2] <- "L"
Summary_ENE_rating$quartile[Summary_ENE_rating$quartile==3] <- "H"
Summary_ENE_rating$quartile[Summary_ENE_rating$quartile==4] <- "HH"

write.csv(Summary_ENE_rating, file = "Summary_ENE_rating.csv", row.names=TRUE)


############### Total Energy/Heat Loss ##############################################

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




jpeg('C:/Warwick_copy/plots/energy_vs_heat_loss_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_heat_loss)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()


#################################################################################

ENE_heat_loss <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(Heat_loss, 4)) %>% 
  ungroup()


DATA_ENE_heat_loss <- ENE_heat_loss %>% 
  arrange(cluster, quartile, Heat_loss) %>% 
  select(flat_name, cluster,quartile, Heat_loss)

DATA_ENE_heat_loss$quartile[DATA_ENE_heat_loss$quartile==1] <- "LL"
DATA_ENE_heat_loss$quartile[DATA_ENE_heat_loss$quartile==2] <- "L"
DATA_ENE_heat_loss$quartile[DATA_ENE_heat_loss$quartile==3] <- "H"
DATA_ENE_heat_loss$quartile[DATA_ENE_heat_loss$quartile==4] <- "HH"


write.csv(DATA_ENE_heat_loss, file = "DATA_ENE_heat_loss.csv", row.names=TRUE)


Summary_ENE_heat_loss <- ENE_heat_loss %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_heat_loss$quartile[Summary_ENE_heat_loss$quartile==1] <- "LL"
Summary_ENE_heat_loss$quartile[Summary_ENE_heat_loss$quartile==2] <- "L"
Summary_ENE_heat_loss$quartile[Summary_ENE_heat_loss$quartile==3] <- "H"
Summary_ENE_heat_loss$quartile[Summary_ENE_heat_loss$quartile==4] <- "HH"

write.csv(Summary_ENE_heat_loss, file = "Summary_ENE_heat_loss.csv", row.names=TRUE)



################## Total Energy/Roof_type ##########################################


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




jpeg('C:/Warwick_copy/plots/energy_vs_energy_roofType_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_roof)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()




#################################################################################

ENE_roof_type <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(energy_roof, 4)) %>% 
  ungroup()


DATA_ENE_roof_type <- ENE_heat_loss %>% 
  arrange(cluster, quartile, energy_roof) %>% 
  select(flat_name, cluster,quartile, energy_roof)

DATA_ENE_roof_type$quartile[DATA_ENE_roof_type$quartile==1] <- "LL"
DATA_ENE_roof_type$quartile[DATA_ENE_roof_type$quartile==2] <- "L"
DATA_ENE_roof_type$quartile[DATA_ENE_roof_type$quartile==3] <- "H"
DATA_ENE_roof_type$quartile[DATA_ENE_roof_type$quartile==4] <- "HH"

write.csv(DATA_ENE_roof_type, file = "DATA_ENE_roof_type.csv", row.names=TRUE)


Summary_ENE_roof_type <- ENE_roof_type %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_roof_type$quartile[Summary_ENE_roof_type$quartile==1] <- "LL"
Summary_ENE_roof_type$quartile[Summary_ENE_roof_type$quartile==2] <- "L"
Summary_ENE_roof_type$quartile[Summary_ENE_roof_type$quartile==3] <- "H"
Summary_ENE_roof_type$quartile[Summary_ENE_roof_type$quartile==4] <- "HH"

write.csv(Summary_ENE_roof_type, file = "Summary_ENE_roof_type.csv", row.names=TRUE)


################### Total Energy/Energy_predicted_density #####################

jpeg('C:/Warwick_copy/plots/energy_vs_energy_predicted_density_clusters.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>% 
  ggplot(aes(energy_predicted, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

par(oldpar)
dev.off()



jpeg('C:/Warwick_copy/plots/energy_vs_energy_predicted_density.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

data_epc_post_cluster %>%  ### without clustering
  ggplot(aes(energy_predicted)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

par(oldpar)
dev.off()

#################################################################################

ENE_predicted <- data_epc_post_cluster %>% 
  group_by(cluster) %>% 
  mutate(quartile = ntile(energy_predicted, 4)) %>% 
  ungroup()


DATA_ENE_predicted <- ENE_predicted %>% 
  arrange(cluster, quartile, energy_predicted) %>% 
  select(flat_name, cluster,quartile, energy_predicted)

DATA_ENE_predicted$quartile[DATA_ENE_predicted$quartile==1] <- "LL"
DATA_ENE_predicted$quartile[DATA_ENE_predicted$quartile==2] <- "L"
DATA_ENE_predicted$quartile[DATA_ENE_predicted$quartile==3] <- "H"
DATA_ENE_predicted$quartile[DATA_ENE_predicted$quartile==4] <- "HH"

write.csv(DATA_ENE_predicted, file = "DATA_ENE_predicted.csv", row.names=TRUE)


Summary_ENE_predicted <- ENE_predicted %>%  
  group_by(cluster,quartile) %>% 
  summarise(n = n())

Summary_ENE_predicted$quartile[Summary_ENE_predicted$quartile==1] <- "LL"
Summary_ENE_predicted$quartile[Summary_ENE_predicted$quartile==2] <- "L"
Summary_ENE_predicted$quartile[Summary_ENE_predicted$quartile==3] <- "H"
Summary_ENE_predicted$quartile[Summary_ENE_predicted$quartile==4] <- "HH"


write.csv(Summary_ENE_predicted, file = "Summary_ENE_predicted.csv", row.names=TRUE)

#############################################################################

# input_data <- read.csv("C:/Warwick_copy/Input_radial_sample.csv")
input_data_OCC <- read.csv("C:/Warwick_copy/Input_radial_sample_OCC.csv")
input_data_FACT <- read.csv("C:/Warwick_copy/Input_radial_sample_Factors.csv")

plot <- Highcharts$new()
plot$chart(polar = TRUE, type = "line",height=500)
plot$xAxis(categories=input_data_OCC$variable_name, tickmarkPlacement= 'on', lineWidth= 0)
plot$yAxis(gridLineInterpolation= 'circle', lineWidth= 0, min= 0,max=5,endOnTick=T,tickInterval=0.5)

plot$series(data = input_data_OCC[,"flat_21_cluster_1_LL"],name = "flat_21 (cluster 1 LL)", pointPlacement="on")
plot$series(data = input_data_OCC[,"flat_90_cluster_2_LL"],name = "flat_90 (cluster 2 LL)", pointPlacement="on")
plot$series(data = input_data_OCC[,"flat_12_cluster_3_LL"],name = "flat_12 (cluster 3 LL)", pointPlacement="on")

# plot$series(data = input_data_OCC[,"flat_23_cluster_1_L"],name = "flat_23 (cluster 1 L)", pointPlacement="on")
# plot$series(data = input_data_OCC[,"flat_32_cluster_2_L"],name = "flat_32 (cluster 2 L)", pointPlacement="on")
# plot$series(data = input_data_OCC[,"flat_16_cluster_3_L"],name = "flat_16 (cluster 3 L)", pointPlacement="on")

# plot$series(data = input_data_OCC[,"flat_25_cluster_1_H"],name = "flat_25 (cluster 1 H)", pointPlacement="on")
# plot$series(data = input_data_OCC[,"flat_73_cluster_2_H"],name = "flat_73 (cluster 2 H)", pointPlacement="on")
# plot$series(data = input_data_OCC[,"flat_65_cluster_3_H"],name = "flat_65 (cluster 3 H)", pointPlacement="on")

plot$series(data = input_data_OCC[,"flat_86_cluster_1_HH"],name = "flat_86 (cluster 1 HH)", pointPlacement="on")
plot$series(data = input_data_OCC[,"flat_13_cluster_2_HH"],name = "flat_13 (cluster 2 HH)", pointPlacement="on")
plot$series(data = input_data_OCC[,"flat_28_cluster_3_HH"],name = "flat_28 (cluster 3 HH)", pointPlacement="on")

plot

############################################################


plot <- Highcharts$new()
plot$chart(polar = TRUE, type = "line",height=500)
plot$xAxis(categories=input_data_FACT$variable_name, tickmarkPlacement= 'on', lineWidth= 0)
plot$yAxis(gridLineInterpolation= 'circle', lineWidth= 0, min= 0,max=50,endOnTick=T,tickInterval=10)

plot$series(data = input_data_FACT[,"flat_21_cluster_1_LL"],name = "flat_21 (cluster 1 LL)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_90_cluster_2_LL"],name = "flat_90 (cluster 2 LL)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_12_cluster_3_LL"],name = "flat_12 (cluster 3 LL)", pointPlacement="on")

plot$series(data = input_data_FACT[,"flat_23_cluster_1_L"],name = "flat_23 (cluster 1 L)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_32_cluster_2_L"],name = "flat_32 (cluster 2 L)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_16_cluster_3_L"],name = "flat_16 (cluster 3 L)", pointPlacement="on")

plot$series(data = input_data_FACT[,"flat_25_cluster_1_H"],name = "flat_25 (cluster 1 H)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_73_cluster_2_H"],name = "flat_73 (cluster 2 H)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_65_cluster_3_H"],name = "flat_65 (cluster 3 H)", pointPlacement="on")

plot$series(data = input_data_FACT[,"flat_86_cluster_1_HH"],name = "flat_86 (cluster 1 HH)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_13_cluster_2_HH"],name = "flat_13 (cluster 2 HH)", pointPlacement="on")
plot$series(data = input_data_FACT[,"flat_28_cluster_3_HH"],name = "flat_28 (cluster 3 HH)", pointPlacement="on")

plot

