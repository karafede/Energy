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

# https://www2.warwick.ac.uk/services/accommodation/staff/all/campus/lakeside/


# Energy consumption data ---------------------------
# Load data
data_energy <- read.csv("university_of_warwick_energy_data.csv.bz2")
names(data_energy) <- str_to_lower(names(data_energy))

# Get a matrix of identifiers
matrix_name <- str_split_fixed(data_energy$name, pattern = " ", n = 5)

# Clean variable names
variable <- matrix_name[, 5]
variable <- ifelse(grepl("elec", variable, ignore.case = TRUE), "electricity", 
                   variable)
# Will catch gas
variable <- str_to_lower(variable)

# Clean and transform
data_energy_clean <- data_energy %>% 
  mutate(flat_number_unknown = matrix_name[, 1], 
         flat_number_unknown = as.numeric(flat_number_unknown),
         flat_name = str_c(matrix_name[, 3], matrix_name[, 4], sep = "_"), 
         flat_name = tolower(flat_name), 
         flat_number = matrix_name[, 4],
         flat_number = as.numeric(flat_number), 
         variable = variable, 
         date = dmy(date),
         unit = str_to_lower(unit), 
         unit = str_replace_all(unit, " ", "_"),
         status = str_to_lower(status)) %>% 
  select(date,
         flat_name, 
         flat_number,
         meter, 
         variable,
         value = periodvalue, 
         unit,
         status,
         flat_number_unknown) %>% 
  arrange(flat_number,
          variable,
          date)

data_energy_tidy <- data_energy_clean %>% 
  select(-unit, -status, -meter, -flat_number_unknown) %>% 
  spread(variable, value) %>% 
  arrange(flat_number, date)

# Plot
data_energy_tidy %>% 
  ggplot(aes(date, gas, colour = flat_name)) + geom_line()

data_energy_tidy %>% 
  ggplot(aes(date, electricity, colour = flat_name)) + geom_line()

summary_flat <- data_energy_tidy %>% 
  group_by(flat_name) %>% 
  summarise(gas = mean(gas, na.rm = TRUE), 
            electricity = mean(electricity, na.rm = TRUE))

summary_flat %>% 
  ggplot(aes(flat_name, electricity, fill = flat_name)) + geom_bar(stat = "identity")

summary_flat %>% 
  ggplot(aes(flat_name, gas, fill = flat_name)) + geom_bar(stat = "identity")

# Export
# write.csv(data_energy_tidy)


# # Insert into database
# # Connect
# db <- threadr::db_connect("C:/Dropbox/ricardo/.database_connections.json", "edis")
# 
# # Insert
# # DBI::dbSendQuery(db, "DROP TABLE warwick_edis_data")
# db_insert(db, "university_of_warwick_energy_data", data_warwick_clean)
# 
# # Check
# data_check <- DBI::dbGetQuery(db, "SELECT * FROM university_of_warwick_energy_data LIMIT 10")



# Occupancy data ---------------------------

# Function to tidy data
tidy <- function (df, date_start_document = "2014-08-01", 
                  date_end_document = "2015-07-31") {
  
  # Get and clean headers
  headers <- df[2, ]
  headers <- str_replace_all(headers, "\\.|\\(|\\)", "")
  headers <- make.names(headers, unique = TRUE)
  headers <- str_replace_all(headers, "\\.", "_")
  headers <- str_to_lower(headers)
  
  # Give table headers
  df <- df[-1:-2, ]
  names(df) <- headers
  
  # Select piecs of the table
  df_one <- df %>% 
    select(1:2, 3:5)
  
  df_two <- df %>% 
    select(1:2, contains("_1"))
  
  names(df_two) <- gsub("\\d", "", names(df_two))
  names(df_two) <- gsub("_$", "", names(df_two))
  
  df_three <- df %>% 
    select(1:2, contains("_2"))
  
  names(df_three) <- gsub("\\d", "", names(df_three))
  names(df_three) <- gsub("_$", "", names(df_three))
  
  df_four <- df %>% 
    select(1:2, contains("_3"))
  
  names(df_four) <- gsub("\\d", "", names(df_four))
  names(df_four) <- gsub("_$", "", names(df_four))
  
  # Bind the selected tables together
  df_bind <- df_one %>% 
    rbind.fill(df_two, df_three, df_four)
  
  # Change data types
  df_bind[1:4] <- lapply(df_bind[1:4], as.numeric)
  
#   df_bind <- df_bind %>% 
#     filter(apartment_no == 74)
  
  # Separate the vacant dates
  df_bind <- df_bind %>% 
    mutate(date_start_vacant = 
             str_split_fixed(vacant_period_if_applicable, n = 2, pattern = "-")[, 1], 
           date_end_vacant = 
             str_split_fixed(vacant_period_if_applicable, n = 2, pattern = "-")[, 2]) %>% 
    select(-vacant_period_if_applicable) 
  
  # Push dates around so an observation had min and max date
  df_bind <- df_bind %>% 
    group_by(apartment_no, no_of_bedrooms) %>% 
    mutate(date_start = lag(date_end_vacant), 
           date_start = ifelse(is.na(date_start), date_start_document, date_start),
           date_end = date_start_vacant, 
           date_end = ifelse(date_end == "", date_end_document, date_end),
           date_start = parse_date_time(date_start, c("ymd", "dmy")),
           date_end = parse_date_time(date_end, c("ymd", "dmy"))) %>% 
    select(-date_start_vacant, -date_end_vacant) %>% 
    ungroup() %>% 
    arrange(apartment_no)
  
  # Seems to be a three day or so offset, shouldn't matter too much but 
  # might need to be looked at
  
  # Return
  df_bind
  
}


# Load data
file_name <- "university_of_warwick_occupancy_data.xlsx"
readxl::excel_sheets(file_name)

# Load sheet, plyr failed for some reason
data_lake <- readxl::read_excel(file_name, "LAKESIDE APARTMENTS", col_names = FALSE)
data_lake <- data.frame(data_lake)

# Make tidy data
data_lake_tidy <- tidy(data_lake)
data_lake_tidy$apartment_name <- "lakeside"

# Load
data_heron <- readxl::read_excel(file_name, "HERONBANK APARTMENTS", col_names = FALSE)
data_heron <- data.frame(data_heron)

# Make tidy data
data_heron_tidy <- tidy(data_heron)
data_heron_tidy$apartment_name <- "heronbank" 

# Bind
data_occupancy <- data_lake_tidy %>% 
  rbind(data_heron_tidy) %>% 
  rename(occupant_adult = number_of_adult_occupants,
         occupant_child = number_of_children_occupants,
         flat_number = apartment_no, 
         bedrooms = no_of_bedrooms) %>% 
  mutate(occupant_sum = rowSums(cbind(occupant_adult, occupant_child), na.rm = TRUE),
         date_end = date_end + hours(23) + minutes(59) + seconds(59)) %>% 
  filter(occupant_sum != 0) %>% 
  add_rownames() %>% 
  mutate(rowname = as.numeric(rowname))

# Conditional join on rowname so occupancy data can be joined
# Standard data frame
data_occupancy <- base_df(data_occupancy)

# Pre-allocate
data_energy_tidy[, "rowname"] <- NA

# Conditional join row name as a key variable
for (i in 1:nrow(data_occupancy)) {
  
  data_energy_tidy[, "rowname"] <- ifelse(
    data_energy_tidy[, "flat_number"] == data_occupancy[, "flat_number"][i] &
      data_energy_tidy[, "date"] <= data_occupancy[, "date_end"][i] & 
      data_energy_tidy[, "date"] >= data_occupancy[, "date_start"][i],
    
    data_occupancy[, "rowname"][i], data_energy_tidy[, "rowname"])
  
}

# Join things with rowname
data_energy_tidy <- data_energy_tidy %>% 
  left_join(data_occupancy, c("rowname", "flat_number"))

# # Check join
# a <- data_energy_tidy %>% 
#   filter(flat_number == 74)

# Drop unneded things
data_energy_tidy <- data_energy_tidy %>% 
  select(-rowname,
         -date_start, 
         -date_end, 
         -apartment_name)

# Push bedrooms forwards, this does not overlap with other apartments
data_energy_tidy <- data_energy_tidy %>% 
  mutate(bedrooms = locf(bedrooms))

# Replace NAs with zeros
data_energy_tidy[, 7:9] <- lapply(data_energy_tidy[, 7:9], function (x)
  ifelse(is.na(x), 0, x))

# Summarise by occupant numbers
summary_occupants <- data_energy_tidy %>% 
  group_by(occupant_sum) %>% 
  summarise(gas = mean(gas), 
            electricity = mean(electricity))

summary_occupants %>% 
  ggplot(aes(occupant_sum, gas, fill = as.factor(occupant_sum))) + 
  geom_bar(stat = "identity")

summary_occupants %>% 
  ggplot(aes(occupant_sum, electricity, fill = as.factor(occupant_sum))) + 
  geom_bar(stat = "identity")

# Export
write.csv(data_energy_tidy, bzfile("university_of_warwick_energy_and_occupant_tidy_data_joined.csv.bz2"), 
          row.names = FALSE)



# Download pdf epc certificates ---------------------------
url_base <- "http://www2.warwick.ac.uk/services/accommodation/staff/all/campus/lakeside/epc/"

sequence <- 1:50
sequence <- str_c("ls", sequence, ".pdf")

url <- str_c(url_base, sequence)

download <- function (url, output) {
  
  name <- basename(url)
  name <- file.path(output, name)
  
  download.file(url, name, quiet = TRUE)
  
}

l_ply(url, download, output = "epc_certificates/", .progress = "text")


# Coventry's epc data ---------------------------
# Load data
data_epc <- read.csv("../epc_data/coventry_epc_tidy_data.csv.bz2")

data_epc_filter_postcode <- data_epc %>% 
  filter(grepl("CV4 7AL", postcode, ignore.case = TRUE)) %>% 
  arrange(full_address)

data_epc_filter_flats <- data_epc_filter_postcode %>% 
  filter(grepl("lakeside|heronbank", full_address, ignore.case = TRUE)) %>% 
  mutate(address1 = str_replace(address1, "Lakeside Appartments", ""), 
         flat_number = str_replace(address1, "Flat", ""), 
         flat_number = as.numeric(flat_number)) %>% 
  select(-rowname, -address1, -address2, -address3, -region, -local_authority,
         -constitiuency, -county)

# 52.3801, -1.57095

# Add the flat which is missing, manually added
# Load
data_epc_flat_47 <- read.csv("flat_47_epc_data_manually_filled_from_pdf_document.csv")

# Bind
data_epc_filter_flats <- data_epc_filter_flats %>% 
  rbind.fill(data_epc_flat_47) %>% 
  arrange(flat_number)

# Load consumption data
data_energy <- read.csv("university_of_warwick_energy_and_occupant_data_joined.csv.bz2") %>% 
  distinct(flat_name, flat_number) %>% 
  select(flat_name, flat_number, bedrooms)

# Join
data_epc_flats <- data_energy %>% 
  left_join(data_epc_filter_flats, "flat_number") %>% 
  select(-main_fuel)
# Flat 47 was missing and there is no flat 70



# Code some variables to metrics
# Load look-up table for factor levels
data_lookup <- read.csv("epc_factor_index_look_up_table.csv")

# Format look-up tables
data_lookup_walls <- data_lookup %>% 
  select(walls_energy_eff, walls_energy_level) %>% 
  filter(!is.na(walls_energy_level))

data_lookup_roofs <- data_lookup %>% 
  select(roof_energy_eff, roof_energy_level) %>% 
  filter(!is.na(roof_energy_level))

data_lookup_main_heat <- data_lookup %>% 
  select(mainheatc_energy_eff, mainheat_energy_level) %>% 
  filter(!is.na(mainheat_energy_level))

data_lookup_windows <- data_lookup %>% 
  select(windows_energy_eff, windows_energy_level) %>% 
  filter(!is.na(windows_energy_level))

data_lookup_hot_water <- data_lookup %>% 
  select(hot_water_energy_eff, hot_water_energy_level) %>% 
  filter(!is.na(hot_water_energy_level))

# Join the factor levels
data_epc_flats <- data_epc_flats %>% 
  mutate(walls_energy_eff = tolower(walls_energy_eff), 
         walls_energy_eff = str_replace_all(walls_energy_eff, " ", "_"), 
         roof_energy_eff = tolower(roof_energy_eff), 
         roof_energy_eff = str_replace_all(roof_energy_eff, " ", "_"), 
         mainheatc_energy_eff = tolower(mainheatc_energy_eff), 
         mainheatc_energy_eff = str_replace_all(mainheatc_energy_eff, " ", "_"), 
         windows_energy_eff = tolower(windows_energy_eff), 
         windows_energy_eff = str_replace_all(windows_energy_eff, " ", "_"),
         hot_water_energy_eff = tolower(hot_water_energy_eff), 
         hot_water_energy_eff = str_replace_all(hot_water_energy_eff, " ", "_")) %>% 
  left_join(data_lookup_walls, "walls_energy_eff") %>% 
  left_join(data_lookup_roofs, "roof_energy_eff") %>% 
  left_join(data_lookup_main_heat, "mainheatc_energy_eff") %>% 
  left_join(data_lookup_windows, "windows_energy_eff") %>% 
  left_join(data_lookup_hot_water, "hot_water_energy_eff")

# Make floor number a factor too
data_epc_flats <- data_epc_flats %>% 
  mutate(floor_level_level = ifelse(floor_level == "Ground", 0, NA), 
         floor_level_level = ifelse(floor_level == "1st", 1, floor_level_level), 
         floor_level_level = ifelse(floor_level == "2nd", 2, floor_level_level))


# 
# # Export
# data_epc_flats %>% 
#   write.csv(bzfile("university_of_warwick_epc_tidy_data.csv.bz2"), row.names = FALSE)

