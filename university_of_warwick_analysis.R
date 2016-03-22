# Set-up ---------------------------
# Load packages
library(threadr)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set global options
options(stringsAsFactors = FALSE)

# Set working directory
setwd("C:/Dropbox/ricardo/edis/warwick")
setwd("~/Dropbox/ricardo/edis/warwick")

# Clear all objects
rm(list = ls(all = TRUE))


# Load data ---------------------------
data_energy <- read.csv("university_of_warwick_energy_and_occupant_data_joined.csv.bz2") %>% 
  mutate(date = ymd(date))

data_epc <- read.csv("university_of_warwick_epc_data.csv.bz2")

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

# Join
data_epc <- data_epc %>% 
  inner_join(summary_energy, c("flat_name", "flat_number"))


data_epc %>% 
  ggplot(aes(flat_name, energy_rating_current)) + 
  geom_point(colour = "darkred", size = 4)

data_epc %>% 
  ggplot(aes(energy_rating_current)) + 
  geom_density(fill = "dodgerblue", alpha = .5)

data_epc %>% 
  ggplot(aes(log(energy_rating_current))) + 
  geom_density(fill = "dodgerblue", alpha = .5)

data_epc %>% 
  ggplot(aes(flat_name, environment_impact_current)) + 
  geom_point(colour = "darkgreen", size = 4)

data_epc %>% 
  ggplot(aes(environment_impact_current)) + 
  geom_density(fill = "green", alpha = .5)

data_epc %>% 
  ggplot(aes(log(environment_impact_current))) + 
  geom_density(fill = "green", alpha = .5)

data_epc %>% 
  ggplot(aes(flat_name, total_floor_area)) + 
  geom_point(colour = "blue", size = 4)


data_epc %>% 
  ggplot(aes(occupant_sum, electricity)) + geom_point(colour = "red", size = 4) + 
  stat_smooth()

data_epc %>% 
  ggplot(aes(occupant_sum, gas)) + geom_point(colour = "orange", size = 4) + 
  stat_smooth()

data_energy %>% 
  group_by(occupant_sum) %>% 
  summarise(electricity = mean(electricity),
            gas = mean(gas)) %>% 
  gather(key, value, -occupant_sum) %>% 
  ggplot(aes(occupant_sum, value, colour = as.factor(occupant_sum), group = 1)) + 
  geom_point(size = 5) + facet_wrap("key", scales = "free_y") + 
  stat_smooth(method = "lm")





# Select a few variables
data_epc_select <- data_epc %>% 
  select(flat_name, 
         bedrooms,
         energy_rating_current,
         environment_impact_current,
         energy_consumption_current, 
         co2_emissions_current, 
         total_floor_area, 
         mainheat_energy_level,
         windows_energy_level,
         hot_water_energy_level,
         floor_level_level) %>% 
  na.omit()
# roof_energy_level
# walls_energy_level,

data_epc_select <- data_epc %>% 
  select(flat_name, 
         bedrooms,
         total_floor_area,
         floor_level_level,
         electricity,
         gas,
         occupant_sum) %>% 
  na.omit()


# Store unique identifiers
rownames <- data_epc_select$flat_name

# Give data frame unique row names
row.names(data_epc_select) <- rownames
data_epc_select$flat_name <- NULL

# Check data
tidy_summary(data_epc_select) %>% 
  select(data_variable, mean, sd, median) %>% 
  gather(variable, value, -data_variable) %>% 
  ggplot(aes(data_variable, value, colour = variable)) + geom_point(size = 4) + 
  facet_wrap("variable", scales = "free_y")

# Standardise variables
data_epc_select_standard <- standardise(data_epc_select)

# Check again
tidy_summary(data_epc_select_standard) %>% 
  select(data_variable, mean, sd, median) %>% 
  gather(variable, value, -data_variable) %>% 
  ggplot(aes(data_variable, value, colour = variable)) + geom_point(size = 4) + 
  facet_wrap("variable", scales = "free_y")


# Calculate distance matrix
distance <- dist(data_epc_select_standard, method = "euclidean")

# Do a Ward hierarchical cluster analysis
fit <- hclust(distance, method = "ward.D")

# Dendogram
plot(fit, labels = FALSE)

# Create a cluster vector
# Clusters
k <- 4

# Group
cluster <- cutree(fit, k = k)

# Plot
rect.hclust(fit, k = k, border = "red")
# graphics.off()

# Give observations cluster variable
data_epc_post_cluster <- data_epc_select %>% 
  mutate(flat_name = rownames,
         cluster = unname(cluster)) %>% 
  arrange(cluster)

# Join cluster group to data
# Select
data_epc_post_cluster <- data_epc_post_cluster %>% 
  select(flat_name, cluster)

# Join
data_epc_post_cluster <- data_epc %>% 
  inner_join(data_epc_post_cluster, "flat_name")



data_epc_post_cluster %>% 
  ggplot(aes(flat_name, electricity, colour = as.factor(cluster))) + 
  geom_point(size = 4) + facet_wrap("cluster")

data_epc_post_cluster %>% 
  ggplot(aes(electricity, fill = as.factor(cluster), 
             colour = as.factor(cluster))) + 
  geom_density(alpha = 0.5)

