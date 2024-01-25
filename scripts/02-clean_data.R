# This file cleans the raw data from ./01-download_data.R and stores it appropriately for use in this paper
library(arrow)
library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)


# reading from the parquet files & making a dataset for each
june_data <- read_parquet("inputs/data/unedited_bikeshare_june_data.parquet")
july_data <- read_parquet("inputs/data/unedited_bikeshare_july_data.parquet")
august_data <- read_parquet("inputs/data/unedited_bikeshare_august_data.parquet")

# cleaning the data and also renaming the columns
# 1. June
june_data_clean <-
  clean_names(june_data) |>
  select(bike_share_ridership_2023_06_csv_trip_id, bike_share_ridership_2023_06_csv_trip_duration, 
         bike_share_ridership_2023_06_csv_start_time, bike_share_ridership_2023_06_csv_start_station_name,
         bike_share_ridership_2023_06_csv_end_time, bike_share_ridership_2023_06_csv_end_station_name,
         bike_share_ridership_2023_06_csv_user_type) |> 
  set_names("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name",
            "user_type") |>
  mutate(duration = duration / 100)

# 2. July
july_data_clean <-
  clean_names(july_data) |>
  select(bike_share_ridership_2023_07_csv_trip_id, bike_share_ridership_2023_07_csv_trip_duration, 
         bike_share_ridership_2023_07_csv_start_time, bike_share_ridership_2023_07_csv_start_station_name,
         bike_share_ridership_2023_07_csv_end_time, bike_share_ridership_2023_07_csv_end_station_name,
         bike_share_ridership_2023_07_csv_user_type) |> 
  set_names("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name",
            "user_type") |>
  mutate(duration = duration / 100)

# 3. August
august_data_clean <-
  clean_names(august_data) |>
  select(bike_share_ridership_2023_08_csv_trip_id, bike_share_ridership_2023_08_csv_trip_duration, 
         bike_share_ridership_2023_08_csv_start_time, bike_share_ridership_2023_08_csv_start_station_name,
         bike_share_ridership_2023_08_csv_end_time, bike_share_ridership_2023_08_csv_end_station_name,
         bike_share_ridership_2023_08_csv_user_type) |> 
  set_names("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name",
            "user_type") |>
  mutate(duration = duration / 100)

# Creating separate datasets for Annual and Casual Members for each month
# 1. June
june_annual_data <- subset(june_data_clean, user_type == "Annual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

june_casual_data <- subset(july_data_clean, user_type == "Casual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# 2. July
july_annual_data <- subset(july_data_clean, user_type == "Annual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

july_casual_data <- subset(july_data_clean, user_type == "Casual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# 3. August
august_annual_data <- subset(august_data_clean, user_type == "Annual Member" &
                               end_station_name != "NULL" & 
                               start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

august_casual_data <- subset(august_data_clean, user_type == "Casual Member" & 
                               end_station_name != "NULL" & 
                               start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 


# Section 1: Popular stations by Member types
# Starting stations: Annual Members
# 1. June
june_annual_start_frequency <- 
  june_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "June")

# 2. July
july_annual_start_frequency <- 
  july_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "July")

# 3. August
august_annual_start_frequency <- 
  august_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "August")

#  Starting stations: Casual Members
# 1. June
june_casual_start_frequency <- 
  june_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "June")

# 2. July

july_casual_start_frequency <- 
  july_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "July")

# 3. August
august_casual_start_frequency <- 
  august_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "August")


# Ending stations: Annual Members
# 1. June
june_annual_end_frequency <- 
  june_annual_data |>
  group_by(end_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "June")

# 2. July
july_annual_end_frequency <- 
  july_annual_data |>
  group_by(end_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "July")

# 3. August
august_annual_end_frequency <- 
  august_annual_data |>
  group_by(end_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "August")


# Ending stations: Casual Members
# 1. June
june_casual_end_frequency <- 
  june_annual_data |>
  group_by(end_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "June")

# 2. July
july_casual_end_frequency <- 
  july_annual_data |>
  group_by(end_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "July")

# 3. August
august_casual_end_frequency <- 
  august_annual_data |>
  group_by(end_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "August")


# left out for now!
# Get MONTHLY common stations for both users
# Starting stations, JUNE
june_most_used_common_start_station <-
  intersect(june_annual_start_frequency$start_station_name,
            june_casual_start_frequency$start_station_name)

# Starting stations, JULY
july_most_used_common_start_station <-
  intersect(july_annual_start_frequency$start_station_name,
            july_casual_start_frequency$start_station_name)

# Ending stations, JUNE
june_most_used_common_end_station <-
  intersect(june_annual_end_frequency$end_station_name,
            june_casual_end_frequency$end_station_name)


# Section 2: Popular start times by Member types
# Annual Member start times
# 1. June
june_annual_data$start_time <- 
  strptime(june_annual_data$start_time, format="%m/%d/%Y %H:%M")
june_annual_data$start_by_hour <- format(june_annual_data$start_time, "%H")

june_annual_hourly_frequency <- 
  june_annual_data |>
  group_by(start_by_hour) |>
  summarise(count_s = n())   

# 2. July
july_annual_data$start_time <- 
  strptime(july_annual_data$start_time, format="%m/%d/%Y %H:%M")
july_annual_data$start_by_hour <- format(july_annual_data$start_time, "%H")

july_annual_hourly_frequency <- 
  july_annual_data |>
  group_by(start_by_hour) |>
  summarise(count_s = n()) 

# 3. August
august_annual_data$start_time <- 
  strptime(august_annual_data$start_time, format="%m/%d/%Y %H:%M")
august_annual_data$start_by_hour <- format(august_annual_data$start_time, "%H")

august_annual_hourly_frequency <- 
  august_annual_data |>
  group_by(start_by_hour) |>
  summarise(count_s = n()) 

# Casual Member start times
# 1. June
june_casual_data$start_time <- 
  strptime(june_casual_data$start_time, format="%m/%d/%Y %H:%M")
june_casual_data$start_by_hour <- format(june_casual_data$start_time, "%H")

june_casual_hourly_frequency <- 
  june_casual_data |>
  group_by(start_by_hour) |>
  summarise(count_s = n())   

# 2. July
july_casual_data$start_time <- 
  strptime(july_casual_data$start_time, format="%m/%d/%Y %H:%M")
july_casual_data$start_by_hour <- format(july_casual_data$start_time, "%H")

july_casual_hourly_frequency <- 
  july_casual_data |>
  group_by(start_by_hour) |>
  summarise(count_s = n()) 

# 3. August
august_casual_data$start_time <- 
  strptime(august_casual_data$start_time, format="%m/%d/%Y %H:%M")
august_casual_data$start_by_hour <- format(august_casual_data$start_time, "%H")

august_casual_hourly_frequency <- 
  august_casual_data |>
  group_by(start_by_hour) |>
  summarise(count_s = n()) 


# Section 3: For each of the 3 summer months, which user is most likely to use the service
june_user_count_stats <- 
  june_data_clean |> 
  group_by(user_type) |>
  summarise(total = n()) |>
  mutate(month = "June")

july_user_count_stats <- 
  july_data_clean |> 
  group_by(user_type) |>
  summarise(total = n()) |>
  mutate(month = "July")

august_user_count_stats <- 
  august_data_clean |> 
  group_by(user_type) |>
  summarise(total = n()) |>
  mutate(month = "August")





