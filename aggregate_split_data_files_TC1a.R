# Load packages
library(threadr)
library(readr)
library(openair)
library(ggplot2)

# Global options
options(stringsAsFactors = FALSE)

# Set working directory
# setwd("~/Dropbox/ricardo/edis/energy_data_splitting")

#### personal hard driver #################################
setwd("E:/NetworkRevolution/TC1a")

# Clear all objects
rm(list = ls(all = TRUE))

# Define function ########################################
##########################################################

summarise_energy_files_hour <- function (file) {
  
  # Load data
  df <- readr::read_csv(file, progress = FALSE)
  
  # Clean names
  names(df) <- str_replace_all(names(df), " ", "_")
  names(df) <- str_to_lower(names(df))
  
  # Filter, transform and aggregate
  df <- df %>% 
    mutate(date = dmy_hms(date_and_time_of_capture, tz = "UTC"),
           hour = hour(date),
           date = date) %>% 
    group_by(location_id,
             measurement_description,
             date,
             hour) %>%
    summarise(sum_Electricity = sum(parameter, na.rm = TRUE)) %>%
    ungroup()
  
  # Return
  df
  
}

####################################################################
######## Zipped files ###############################################

# setwd("E:/NetworkRevolution/TC1a/zip_split")
# 
# file_list <- list.files(path = "E:/NetworkRevolution/TC1a/zip_split/", 
#                         full.names = TRUE)

####################################################################


# List files
# file_list <- list.files(path = "/media/stuartg/MOBILE_II/energy/data/split/", 
#                         full.names = TRUE)

file_list <- list.files(path = "E:/NetworkRevolution/TC1a/data_split/", 
                        full.names = TRUE)

file <- file_list[1]
AAA <- read_csv(file)
  headers <- colnames(AAA)
  head(AAA)
  head(AAA[4])

# Apply function to all files
# data_summary <- ldply(file_list, summarise_energy_files_hour, .progress = "time")
data_summary <- ldply(file, summarise_energy_files_hour, .progress = "time")
# write.csv(data_summary, "E:/NetworkRevolution/TC1a/Electricity_data_hour.csv", row.names = FALSE)
data_summary <- data_summary[1:100,]

data_summary$date <- str_sub(data_summary$date, start = 1, end = -10)

# Ensure all days are accounted for only once to avoid dupicates

data_summary_hour <- data_summary %>% 
  group_by(location_id,
           measurement_description,
           hour,
           date) %>% 
  summarise(sum_Electricity = sum(sum_Electricity, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(location_id,
          hour,
          date)

## Plot data 
data_summary_hour %>% filter(location_id == 7823)  %>% ggplot(aes(hour, sum_Electricity)) + geom_line()


# Export
write.csv(data_summary_hour, "E:/NetworkRevolution/TC1a/Electricity_data_hour.csv", row.names = FALSE)

### Join Customer Cell definition data #####################################

Test_Cell <- read_csv("E:/NetworkRevolution/TC1a/CustomerTestCellDefinition.csv")
Electricity_data <- read_csv("E:/NetworkRevolution/TC1a/Electricity_data_hour.csv")


#### Group by location_id (user ID) ###################

# Electricity_by_location <- Electricity_data %>% 
#   group_by(location_id) %>% 
#   summarise(Sum_Electricity = sum(value))%>%
#   ungroup() %>% 
#   arrange(location_id)
Electricity_data <- subset(Electricity_data, !is.na(sum_Electricity))
 
 ### Rename classes names ####################################################
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("A Alpha Territory", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Alpha Territory", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("B Professional Rewards", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Professional Rewards", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("C Rural Solitude", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Rural Solitude", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("D Small Town Diversity", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Small Town Diversity", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("E Active Retirement", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Active Retirement", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("F Suburban Mindsets", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Suburban Mindsets", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("G Careers AND Kids", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Careers AND Kids", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("H NEW Homemakers", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "NEW Homemakers", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("I Ex-Council Community", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Ex-Council Community", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("J Claimant Cultures", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Claimant Cultures", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("K UPPER FLOOR Living", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "UPPER FLOOR Living", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("L Elderly Needs", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Elderly Needs", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("M Industrial Heritage", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Industrial Heritage", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("N Terraced Melting Pot", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Terraced Melting Pot", Test_Cell$`Mosaic Class`)
 
 Test_Cell$`Mosaic Class`<- ifelse(grepl("O Liberal Opinions", Test_Cell$`Mosaic Class`, ignore.case = TRUE), 
                                   "Liberal Opinions", Test_Cell$`Mosaic Class`)

 headers <- colnames(Test_Cell)
 headers <- str_replace_all(headers, " ", "_")
 headers <- str_to_lower(headers)
 colnames(Test_Cell) <- headers
 
 ### Consider only TC1a data ###########################
 Test_Cell <- Test_Cell %>%
   filter(test_cell_id == "1a")
 
 ### Join Electricity data consuption with Customer cells data #####
 Electricity_data_Joined <- Test_Cell %>% 
   left_join(Electricity_data, "location_id") 
 
 Electricity_data_Joined <- subset( Electricity_data_Joined, !is.na(sum_Electricity))
 colnames(Electricity_data_Joined)[11] <- "Sum_Electricity_kWh"
 
 ### The Sum Electricity in in kWh
 write.csv(Electricity_data_Joined, "E:/NetworkRevolution/TC1a/Electricity_data_TC1a_hour.csv", row.names = FALSE)
 
 